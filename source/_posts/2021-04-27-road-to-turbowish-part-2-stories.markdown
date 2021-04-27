---
layout: post
title: "Road to TurboWish; Part 2: Stories"
date: 2021-04-27 11:21:32 -0400
comments: false
categories: rust turbowish performance
---
It's story-time!

[poll-tweet]: https://twitter.com/pnkfelix/status/1387022496717807622

> As I walk, I think about a new way to walk

In my [previous post][part1], I described the observations that led us to select performance tooling as a focus, as well as the goals document I wrote to guide the next steps of the project. Now I want to show you the User Stories I wrote, to show what I think our customers want (and can reasonably expect) out of performance tools.

<!-- more -->

Everything that follows was taken verbatim{% sidenote 'tmbg' 'Okay, not *verbatim*. [As promised][poll-tweet], I am spicing things up a little, this time offset outright quotations rather than section headings.' %} from the document I presented to the other members of the AWS Rust team. This document is an attempt to describe what I envisage as awesome developer experiences doing performance investigation. As with [part 1][part1], I will be adding side-commentary on the right-hand margin.

> As I think, I'm using up the time left to think

My next post will present the design document that arose from these and other stories, as well as discussions about the realities of what we can expect to implement.

[part1]: /blog/2021/04/26/road-to-turbowish-part-1-goals/

----

Note: These are meant to be read in order: The first describes some up-front investment that the later stories either 1.) assume other developers already put in, or 2.) in the case of user “Barry”, state explicitly that such investment of effort is deliberately skipped by the developer.


> And this train keeps rolling off the track
>
> Trying to act like something else
>
> Trying to go where it's been uninvited

{% marginblock %}
I jumped right in and started making up a story about a newcomer to async Rust.
I did not actually look at examples of user stories as they are typically used in Agile development, or even really how it is done at AWS. It has been a long time since I had read essays describing "how" to do Agile development; looking at that now, I see that typical user stories are *one or two sentences*. (I'm not sure I can properly convey what I want these tools to do in one or two sentences. I mean, it would be a place to start, but I wanted to spell out the experience. Maybe the reality is that the step I'm doing is *not* a user story. But it *does* seem like it at least falls into the ["Working Backwards" style][working-backwards] that is promoted here at AWS. It also matches the ["shiny future" story writing][shiny-future] approach that Rust's Async Foundations working group has been trail-blazing (apart from the fact that I skipped the step of writing a description of the ["status quo"][status-quo] for each of the stories here).
{% endmarginblock %}

[working-backwards]: https://medium.com/the-digital-project-manager/working-backwards-a-new-version-of-amazons-press-release-approach-to-plan-customer-centric-c17991583508

[shiny-future]: https://rust-lang.github.io/wg-async-foundations/vision/how_to_vision/shiny_future.html

[status-quo]: https://rust-lang.github.io/wg-async-foundations/vision/how_to_vision/status_quo.html

[async-vision-cast]: https://nikomatsakis.github.io/wg-async-foundations/vision/characters.html

## User Story 1: Abigail

Abigail is getting started with async Rust development. She's worked her way through the Async Book and transcribed some example code. Now she's trying to write her first medium sized program, a terminal-based chess program, and it doesn't produce any output after she makes her first move and is waiting for the opponent to move.

After asking for advice about what to do on the Tokio Discord chat, she enables the turbowish feature in her Cargo.toml, and recompiles. She receives warnings from the compiler saying that there are calls to `task::spawn` but no contextual labels anywhere in her crate.

Abigail reads the compiler's suggestions for the kinds of declarations to add, and adds annotations accordingly, labeling her tasks. (Her program is roughly structured according to an actor model, so she has labels like "game AI", "move validator", and "terminal UI").

Abigail reattempts a build, and now notices some of the previous warnings were not addressed in her change and are emited again: These warnings are about missing labels on resources, namely the channels used for communication between the tasks, e.g. "user input", "player move", "opponent move", etc. She adds labels to the channels, and the program now compiles with no diagnostic warning. She runs her Chess program.

The Chess program starts up, and this time it includes, as part of its console output, a socket to connect to, which is serviced by a dedicated Turbowish thread. She runs a separate tokio-console program, connecting it to the advertised socket on the Chess program. Events begin streaming from the TurboWish thread to the `tokio-console`. The `tokio-console` output notes that it has a webserver mode available; she opts into that, and the `tokio-console` spits out a local URL to connect to. Abigail starts up a web browser and connects it to the URL. The rendered page lists the three tasks.

The Chess program again unexpectedly blocks (or, more precisely, livelocks) after she inputs her first move. Abigail looks at the rendered page, and notices a button that says "task dependencies." She clicks it, and the page renders a picture showing a directed graph,{% sidenote 'directed-graph' 'This is the core part of my vision for performance tooling that I strived to preserve when I ported this story into [Barbara Makes A Wish][barbara-wish] on the Async Vision document.' %}
linking tasks and resources (or more generally, wakers), which depicts both 1.) what resources a task is waiting on, and 2.) what tasks are responsible for making those resources available for use.

[barbara-wish]: https://rust-lang.github.io/wg-async-foundations/vision/shiny_future/barbara_makes_a_wish.html

The resulting rendering shows a cycle in the graph that looks like this, where circular nodes are tasks and rectangular nodes are resources shared by the tasks, like channels.

{% marginblock %}
I am breaking my rule about presenting the verbatim text here. In the document I shared with my colleagues, the underlying file didn't support mermaid or any other markdown-oriented diagramming tools. So there I resorted to something like
`(terminal UI)` `->` `[board update]` `->` `(move validator)` `->` `[player move]` `->` `(terminal UI)`
in that document, where parentheses and square brackets represent circle vs square node shapes, differentiating tasks from resources.
But I could not bring myself to subject my readers here to that, when its so easy to put in a mermaid diagram.
{% endmarginblock %}
<!-- The manual height specification is a rough guess, to allow the rest of the doc to predict the space occupied by the graph once it is rendered. -->
<div class="mermaid" style="height: 300px">
graph LR
  AI(("game AI"))
  TUI --> Update --> Validator --> Player --> TUI
  TUI(("terminal UI"))
  Update["board update"]
  Validator(("move validator"))
  Player["player move"]
</div>

Notably, there are no paths in the rendered graph from \[board&nbsp;move\]{% sidenote 'board-move-typo' 'I must have meant for this to say \[board&nbsp;update\]. I had not caught that typo until just now.' %}
to (game&nbsp;AI).

Abigail wonders why the move validator task is waiting for a player move, when she would expect it to be waiting an opponent move from the AI.

She realizes there must be a error in her logic for how either the "move&nbsp;validator" or the "board&nbsp;update" are set up, since she would expect the "game&nbsp;AI" task to be somewhere in the path above.

Moving her mouse over the graph, she notices that the graph edges link to points in her source code. She starts digging in, using the provided links as direction for where to look to figure out what went wrong in her logic.


> And the rain falls down without my help I'm afraid

{% marginblock %}
When I wrote these stories, I felt it would be useful for all of the characters to have names where their first letters corresponded to the letters of the Alphabet: Abigail, Barry, Chris, Daphne. Some feedback I got back almost immediately from Niko Matsakis was "Why didn't you re-use the [characters from the Async Vision cast][async-vision-cast]?" I don't have any great answers to that. (The obvious answer was a sheepish "I haven't read the Async Vision document yet..."; I have corrected that oversight since then.)
{% endmarginblock %}
## User Story 2: Barry

Barry is an experienced tokio developer. He has been asked to help out with an AWS cloud service using tokio. The developers of the service are seeing higher latencies than expected for relatively small service requests.

Barry enables tokio's turbowish feature. He opts not to take the compiler's diagnostic suggestions regarding missing contextual labels and downgrades the diagnostic to `#![allow]` across the crate, knowing that TurboWish will infer labels and attach them automatically based upon crate and module names (and a fixed number of stack frames, when debuginfo is available).

Barry connects `tokio-console` to the cloud service's socket, and chooses its terminal user-interface instead of the webserver mode.

Barry first asks for a live feed of tasks transition between (`running`, `waiting`, `ready`). The feed produces too much output to the terminal, so Barry sends it to a file instead, and then inspects it in a text editor, trying to see trends by eye. It is a lot of data though, too much for Barry to interpret without more tooling.

Barry has dealt with parsing this output before, though.

Barry has a home-grown script to extract a CSV file with numeric data from the output. Barry imports the CSV file into a spreadsheet and then constructs a histogram with buckets of ranges of individual wait-times, and how many tasks’ transition points are in each bucket.

From this, Barry identifies a small set of tasks that spent an unusually long time in the `waiting` state. Going back to the original feed, Barry finds the the records for those long waits, which include a link to the point in the source code where the long waits occurred.

Barry looks, and sees a call to `std::thread::sleep` instead of tokio's sleep function. [TODO: I'd be happy to put in some other
example here of blocking code that should be replaced with non-blocking code.]{% sidenote 'todo-other-examples' 'This TODO was in here during the team document review. The basic response was "eh, std sleep is good enough as an example. It is a real instance of a much more general mistake that people do make, and it gets the idea across."' %}

Barry replaces the call `std::thread::sleep` with tokio's sleep function, and returns to the histogram to see what other instances of unusually long wait times he can find.

> And my lawn gets wet though I've withheld my consent

{% marginblock %}
With the Chris story, we have a sudden shift from async-oriented performance issues to compiler developer performance issues. what can I say other than ["write what you know"][write-what-you-know].
{% endmarginblock %}
{% marginblock %}
(Interestingly, all the essays I can find on that [trope][write-what-you-know-trope] indicate that it is meant to be about knowledge of *inner emotions*, not about facts or experience.)
[write-what-you-know]: https://thewritepractice.com/write-what-you-know/
[write-what-you-know-trope]: https://tvtropes.org/pmwiki/pmwiki.php/Main/WriteWhatYouKnow
{% endmarginblock %}
## User Story 3: Chris

Chris is a contributor to the Rust compiler, `rustc`. Chris is well-versed with Rust, at least for batch programs like the compiler, but they do not do any async Rust development. Chris wants to gain better understanding of memory usage internal to `rustc` itself.

After asking around in the `#compiler-team` Zulip stream, Chris enables the `build.heap_inspector` setting in `rustc`'s build
configuration file, and rebuilds the compiler.

Chris also fires up the TurboWish web frontend. This invocation of TurboWish uses a specialized config file developed and maintained by the Rust compiler team that handles interpreting the types that are special to the compiler.

Chris uses environment variables to indicate which points in the compiler's control flow are of interest to them. More specifically, Chris wants to inspect the heap as it stands immediately after the borrow-checker runs.

The compiler obliges: right after the borrow-checker runs, it starts from a set of known roots (tagged as such ahead of time by the rustc developer) and traverses the ownership graph from those roots,{% sidenote 'gc' 'You can see my garbage-collection background leaking through here. I guess this is an instance of "what what you knew..."' %} emitting a description of the arcs it encounters during its traversal. For each heap-allocated value the traversal encounters, the traversal code also
includes information needed to reconstruct its "payload size"; i.e., enum values will include their discriminant, vectors and string include their length, etc.

The TurboWish web front end receives this stream of graph traversal data. It uses this to provide a table describing the number of objects of each type, and how much internal fragmentation it detects based on the reported payload sizes. Chris inspects this table and determines that that there is an opportunity to reduce fragmentation by changing one of the enums to use a `Box`. Chris tries it out and sees the memory consumption of the compiler go down, and files a pull request
with the change.

(Note: This user story may or may not not provide value over other tools like dhat, depending on 1. whether it allows tracking allocations at a finer grain than malloc calls (e.g. sub-allocations within an arena) and/or 2. how much value one puts on being able to inspect portions of the heap rooted at certain owners. See also [the appendix][appendix], which includes a more ambitious “dream” proposal for Chris, but also one that I personally am not as sure actually pays off in terms of customer value versus mere “coolness” factor.)  

> When this grey world crumbles like a cake

## User Story 4: Daphne

{% marginblock %}
With Daphne's story, we return to async-oriented performance analysis.
Async is a hot topic, and I figured it was worth exploring other stuff we could deliver. In this case, the idea is to turn the internal logging data into a rendered message sequence chart. (I actually do not know if people use message sequence charts on real logs in practice. I have seen them used *very* often for presenting small snippets that explain a protocol, but are they actually useful for traversing a complex series of interactions? I can imagine it could be, especially in an interactive presentation that can highlight links and automatically re-center on a selected end of a given arc in a graph.
{% endmarginblock %}
Daphne is maintaining an existing a web service built atop tokio. She gets reports of poor performance for certain input queries that match the "E" command for her service, which already has TurboWish integrated. In response, she attaches `tokio-console` to her service, and tells it to trace the flow of requests matching the "E" command (more concretely, `[{req.path="/Q"}]=trace`), and then fires up the TurboWish web front-end.

Daphne chooses a tab that says "Show Scheduler." The resulting page looks something like Message Sequence Chart: Each of tokio's executor threads has a vertical line, and as futures are polled, they have corresponding boxes on the corresponding thread. Since Daphne has limited the output to just events related to the "E" command, all the futures she sees being scheduled are part of that query.

From skimming the resulting charts, Daphne sees that when the "E" futures are being polled, they seem to yield again promptly. She does see some evidence the future is migrating to different threads, and she makes a note to try to investigate whether there is a way to ask tokio to avoid such thrashing (and also, whether there are metrics should could gather about whether doing so could help).

There are also points in the program flow when *no* future related to the "E" query is scheduled. The event stream does not include information about which futures are polled during those times. Those portions of the executor threads vertical lines are colored dark green: they may be doing useful work, but its not work directly related to the events that have been filtered in.

Daphne realizes that she needs to find out whether the "E" futures are being left unexecuted because they're still waiting for their corresponding wakers to be invoked, or if the "E" futures are all ready and there is some other issue in the scheduler (or in the other tasks that causes them to starve the executing threads). Daphne goes to `tokio-console` and issues the command requesting the feed of all tasks transitioning between (`running`, `waiting`, `ready`). With that, the message sequence chart now shows that her "E" futures are indeed ready much of the time. She also sees that there are large blocks of time where all of the threads in the pool are running without any interrupt. Daphne hovers the mouse over those rendered blocks of time,  and a pop-up window shows a description of the futures that are being polled at that time. Daphne goes off to investigate that piece of code, and discovers a `for`-loop with some semi-expensive body that never yields to the executor.

# Appendix
[appendix]: #Appendix

> I'll be hanging from the hope
>
> That I'll never see that recipe again

## User Story 3b: Chris's Dream

Chris wants to inspect the heap as it stands immediately after the borrow-checker runs.

The compiler obliges: right after the borrow-checker runs, it pauses and prompts for the user to connect TurboWish. Chris connects the `rustc-console` app and interactively asks what root objects are available to inspect. One of the named objects in the result is the "MirBorrowckCtxt" (i.e. the borrow-checking context). Still working at the terminal `rustc-console`, Chris first asks for how much memory is solely owned, potentially indirectly by the "MirBorrowckCtxt" object. `rustc-console` spits out a number. Chris then asks for how much memory is owned, potentially shared with others (e.g. via `Rc`), by the object. `rustc-console` spits out another, much larger number.

Chris connects the TurboWish web front-end, and then queries for a ownership tree describing the values owned by the "MirBorrowckCtxt" object.

TurboWish does a graph traversal over the owned heap allocated objects, starting from the borrow-checking context (`MirBorrowckCtxt`) as the root, and emits a description of the arcs it encounters during its traversal, as well as well as the data payload for any fields of types that have been previously marked by the rustc developers as of interest.

The TurboWish web front end renders the resulting graph. But in addition to the direct ownership relatinships implied by the Rust language semantics the graph, it also includes arcs for any `&`/`&mut` references in the graph, and it *also* includes some domain-specific dotted arcs for keys into tables that are meant to be interpreted as references. (This of course is only possible due to the rustc-specialized config file that was included when TurboWish was loaded up.)

{% marginblock %}
I think this whole appendix was just an excuse to try to squeeze in a reference to Hyperbolic Trees. Just over twenty years ago, I spent a summer (or was it a year) hacking together code to render a graph via such trees as part of a project called [Footprints][footprints]; the linked paper still has screenshots of it.
{% endmarginblock %}

[footprints]: http://alumni.media.mit.edu/~wex/CHI-99-Footprints.html

However, since the object graph is massive, the rendering does not fit on the window. Chris first toggles an option to change the rendering to use a hyperbolic tree (see https://en.wikipedia.org/wiki/Hyperbolic_tree), where the current object is presented centrally, and immediate children are surrounding smaller nodes, and grand-children are still smaller, et cetera. The hyperbolic tree presentation compacts the view significantly, at the cost of obscuring details of distant descendants and ancestors, as well as cousins.

There is also an option on the presentation UI to have the circles for each node be different sizes depending on how much space they individually occupy on the heap (not including their linked children); Chris enables this setting, just to get a rough visual feeling for where memory space is going in the ownership tree.

----

And that's all the user stories. That's the whole document.

For the sequel, I will be presenting the design document that I think best represents the plan going forward.

We are still figuring things out, to some extent, but I continue to dream about what we can do for our customers, the Rust developers of the world.

> When the word comes down "Never more will be around"
>
> Thought I'll wish we were there, I was less than we could bear
>
> And I'm not the only dust my mother raised
>
> I am not the only dust my mother raised

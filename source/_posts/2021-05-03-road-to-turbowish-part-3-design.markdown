---
layout: post
title: "Road to TurboWish part 3: Design"
date: 2021-05-03 14:29:02 -0400
comments: false
categories: rust turbowish performance
---
This is part three in a series about the TurboWish performance analysis tool suite. In [part one][] I described why our team selected this as a focus, and the goals for the tools. In [part two][] I presented four narratives, each representing different idealized future customer experiences using the tools.

Now I want to show you the design document I developed, which digs more into how I think{% sidenote 'how-i-think' 'I cannot emphasize enough that nothing here is set in stone. These are ideas I am circulating with you precisely so that you can tell me what needs to change.' %} the system should be architected.

<!-- more -->

I will warn you up-front: This is a long post. Eight pages printed in my PDF preview; ten if you include the appendices. And yet it is largely just a transcription of [another doc][the-hackmd]. But the thing it is transcribing is going to evolve with time, and I like the idea of taking a snapshot of its state today. I also think the ideas in it are fun. A lot of it still remains to be designed. Much in here falls into the category of software that you can produce with a ton of dedicated programming on *every level of the software stack*; I want to figure out how to avoid that.

My most important unresolved question is, how can we deliver all of this functionality with *minimal investment* on the part of our end developer who is themselves a newcomer to Async Rust programming.

Everything that follows was taken verbatim{% sidenote 'pink-floyd' 'Okay, not *verbatim*. I am again spicing things up a little by interleaving another favorie song alongside the text of the document, at the risk of turning this post into a Mark Z. Danielewski novel.' %} from [the document][the-hackmd] I presented to the other members of the AWS Rust team. This document is an attempt{% sidenote 'second-attempt' 'There was an earlier iteration of this document, with a slightly different design. I am not going to write a blog post about the earlier document (I figure it is better to jump straight to the superior version), but it is [openly available](https://hackmd.io/QYohB4uTTkas20t6rhCrww), just like the document I have transcribed into this blog post.' %}  to describe a software architecture for the first, most important tool: An "Async Monitor" that models the behavior of one's Rust async runtime, alongside an "Async Console" that presents slices of information from that model. As with parts [one][part one] and [two][part two], I will be adding side-commentary on the right-hand margin.

[poll-tweet]: https://twitter.com/pnkfelix/status/1387022496717807622

[the-hackmd]: https://hackmd.io/WsJg_695SUiH41DLMBRekA
[part one]: /blog/2021/04/26/road-to-turbowish-part-1-goals/
[part two]: /blog/2021/04/27/road-to-turbowish-part-2-stories/


----

## TurboWish Development Plan: The Async Monitor and Console


{% marginblock %}
(This time I did not repeat my ["mistake" of crowd-sourcing][poll-tweet] the choice of source material. If I were a proper magician, I would have employed a force, but I had a bit too much confidence in my ability to find appropriate quotes, even from ABBA for goodness sakes!)
{% endmarginblock %}

> Overhead the albatross
>
> Hangs motionless upon the air
>
> And deep beneath the rolling waves
>
> In labyrinths of coral caves


## TurboWish Overview



TurboWish is a suite of tools that give Rust developers insight into performance issues{% sidenote 'issues-with-issues' '"Performance issues" raises questions immediately. What does "issues" mean? Is this a profiler? A debugger? My current answer: I had not appreciated this until recently, but I think there is a spectrum between profiling and debugging. After all, they both come down to *understanding the behavior a program*. Providing insight into that is what TurboWish is all about.' %} in their code.

The first TurboWish deliverable is the Async Monitor and Console, which answers a developer's questions about how their code's async runtime{% sidenote 'runtime-v-executor' 'I had been using the word "async executor" here, but Carl pointed out to me that the executor is just one small piece of the overall framework provided by a typical async environment. So now it says "runtime." I am curious what other terms might work here as well.' %} is behaving as their program runs.

The Async Console provides a summary of an async runtime's behavior, using concepts shared across the async rust ecosystem (such as tasks and resources), and metrics that are applicable to any async program (such as the average time each task spends waiting in a ready state).

When a developer asks: "Why is my task stuck?" or "Why is my app slow?", the Async Console is the first, and in some cases, the only tool they need to reach for. It will allow the developer to quickly see how tasks are scheduled (to learn how much time is spent in the developer's own code versus waiting to run), identify tasks that are starving or blocked and what resources they are waiting on, and identify tasks responsible for replenishing a scarce resource.
 
We plan to expand the TurboWish tool suite with other tools{% sidenote 'other-tools' 'You may have wondered when reading the [user stories in part two][part two] how a single tool was going to satisfy all those different narratives. The phrase "other tools" here hopefully answers that questions: currently, I am not planning to have a single tool do all of the stuff described in those stores.' %}  dedicated to other investigations, such heap profiling or sampling-based CPU profiling. This design document is dedicated to the Async Monitor and Console; in tandem, they are the tool that will drive the top-down investigation of an async developer's performance issues.

> The echo of a distant time
>
> Comes willowing across the sand
>
> And everything is green and submarine

## Document overview

This document describes the development plan for the first TurboWish tool: the Async Monitor and Console. It opens with the original goals followed by (proposed, unfinalized) [tenets](#Tenets.of.TurboWish.Tool.Suite) for the TurboWish suite itself. It then presents a description of the [customer experience](#The.Async.Console.Experience) using the Async Monitor and Console. Then it sketches an [implementation plan](#Implementation.Plan). A section follows describing [metrics](#Metrics) for evaluating whether the tools are achieving their goal, and finally a section touching on [security concerns](#Security.Concerns). [Appendices](#Appendices) follow the [conclusion](#Conclusion) of the document, including an appendix with the [project schedule](#Schedule).


> And no one showed us to the land
>
> And no one knows the where's or why's
>
> But something stirs and something tries
>
> And starts to climb toward the light


<!-- goals embedded from other doc -->

# Goals

<!-- We don't need this link here; people who want to comment will already be looking at the original hackmd -->
<!--
[link to goals](https://hackmd.io/OcfdnDwKRSiW-6WmSVt0Bw) (if you want to comment on them, go to the linked doc)
-->

*Profile Production Code*: Incorporating the TurboWish Framework is low-overhead: it can be incorporated into production code{% sidenote 'production-code' 'You might ask: Does "production code" mean the same thing as "deployed code running live"? And you would be right to ask that. In hindsight, I should have included an entry about this in the "Minimum Viable Product" section where I described long-term concerns versus short-term sacrifices. My attitude here, in any case, is that there are *other* infrastructures for logging behavior of live services. Integration with those infrastructures might make sense eventually, but I do not regard it as a pre-requisite for launching *this* product.' %}  without producing an undue maintenance burden and without incurring significant performance overhead.

*Domain-specific Feedback*: Frameworks and applications can provide data for specialized metrics, specific to their internal architecture.

*Understand Hidden Costs and Connections*: Frameworks like tokio ease writing asynchronous code because they hide a number of details behind abstractions (such as generator code produced by the Rust compiler, or task queues managed by the tokio runtime). TurboWish exposes those hidden details, allowing developers to correlate them with other program events. It also exposes connections that humans usually have to reconstruct by hand (such as future to resource to future chains that can yield deadlock), allowing one to directly see from Rust’s ownership model how resources are being held in the object graph.

*Framework Agnostic*: Many of Rust’s customers use tokio, but not all of them. async-std  and fuschia_async are other frameworks for asynchronous programming. TurboWish can provide value to any such framework (though it may also provide framework-specific functionality when warranted). For our initial releases, we can focus on tokio alone, but expect integration with others if use with tokio proves successful.

*EC2 Instance Type Agnostic*: If we make use of any OS specific features (e.g. dtrace probes), they will be available on all EC2 AL2 instances, regardless of instance-type. (Specifically, we cannot require access to CPU performance counters for core functionality. We may offer extra features that utilize them.)




{% marginblock %}
This use of the word "tenets" is part of Amazon parlance. When you see it, you can think "Guiding Principles".
{% endmarginblock %}

## Tenets  of TurboWish Tool Suite

The Rust community at large, both within and outside of AWS, is the customer for TurboWish.

Injected instrumentation must not block application progress.

Timing-measurement noise induced by client instrumentation should be minimized.

Present diagnoses in terms of customer-centric concepts, such as resource and task.

Minimize coupling between components to encourage concurrent community development.

The transition from development to production deserves as much performance tooling as production monitoring itself. (More specifically: We see customers struggling to get their software operating well enough to push to release; therefore, some tools can be specialized to the development use-case.)
The Async Monitor is one such tool: It is aimed at programs under development, not in release.


> Strangers passing in the street
>
> By chance, two separate glances meet
>
> And I am you and what I see is me

## The Async Console Experience

When a developer wants to deploy the Async Monitor on their program, they will need to hook it in by adding it as an upstream dependency (in the `Cargo.toml` file) and also writing a few initialization lines at the start of their source code:

```rust
use turbowish_async_monitor as tw_monitor;

#[tokio::main]
async fn main() {
    tw_monitor::builder().port(8080).init();
    
    // Rest of the app
}
```

With that initialization code in place, the service will operate in the same fashion as before, but will now also run the *Async Monitor*, which observes events and then, based on those observations, builds an internal model of the program and the async executor. External programs, such as the *Async Console* can now connect and present the state of the Async Monitor by connecting to the port indicated above..

When the developer first connects the Async Console, they get a presentation similar to the UNIX `top` command, showing a brief summary of the executors (with metrics like the number of tasks running or sleeping, average times spent waiting to run, average `Future::poll` runtimes), and below that, a list of the current tasks, each on its own line with with an id number, name, current run state (Polling, Ready to poll, and Waiting), and a list of task attributes that include developer-specified metadata.

{% marginblock %}
Mock up of the console UI was provided by Carl Lerche, a lead Tokio developer.
{% endmarginblock %}
![async console terminal user interface](https://i.imgur.com/F71IQMl.png)

This view will dynamically update (just like `top`) as the application runs.

The async console presents data primarily oriented around tasks and resources. The aim is for a model as straight-forward to understand as the Resource Allocation Graphs{% sidenote 'holt' 'I will be honest: These task/resource graphs are the part of the system I personally am most excited about. I do not yet know how we will deliver on the developer experience that I envisage, but I want it to be as beautiful as the graphs I presented in [part two][].' %} described by [Ric Holt in 1972][holt72]: We wish to explain as much as we can in terms of tasks waiting for or using resources (either by locking them if they are exclusive resources, or by consuming them if they are cumulative resources such as message channels), or resources waiting for tasks to make them available (either by unlocking them for exclusive resources, or by refueling them if they are cumulative resources).

[holt72]: https://dl.acm.org/doi/10.1145/356603.356607

The async console monitors for known performance pitfalls, and includes a highlighted alert at the top of the screen if the program is currently exhibiting one of those pitfalls. The alert provides a link to a problem-oriented view, that explains the pitfall and provides guidance as to how one can resolve it.

From the async console, the developer can do three main things. First, they can dig into an individual **record** of a task or resource, by traversing hyperlinks for that task or resource. (A "record" in this context is a screen summarizing the event history and current status for that task or resource.) Second, they can **pause** a waiting task (or request that a specific non-waiting task be paused when it next evaluates a `.await`); such paused tasks can be subsequently inspected by a debugger attaching to the running process, and then resumed when the developer is done with the attached debugger. Third, they can **rollback** history, in order to inspect past states for tasks or resources.

> And do I take you by the hand
>
> And lead you through the land
>
> And help me understand the best I can?

### Performance Pitfall Alerts

The performance pitfall alerts handle a collection of known cases where our customers are asking themselves: "why is my task stuck?"

Example of problems that the Async Monitor can detect include: deadlock cycles, excessive polling times, and buggy implementation of `Future` that fail to register a waker.

As a concrete example: When a deadlock cycle occurs, the async console displays an alert saying "deadlock cycle detected." The developer follows the hyperlink for the alert, and this brings up a "problem view" that shows an interleaving list of tasks and resources{% sidenote 'lists-vs-graphs' 'Here I made a concession to reality: users do not want graph diagrams shoved in their faces all the time. Lists are sometimes an entirely reasonable presenation medium, especially when it comes to describing a deadlock cycle.' %}, corresponding to the chain of dependencies that forms the cycle.

### Resource Records

A resource record shows information about an individual resource.

Some resources are *exclusive*: they can be held by at most N tasks at a time (where N is often 1, such as in the case of a mutex lock). The resource record for an exclusive resource will include a list of tasks that currently hold it. It will also include a separate list of tasks that are currently blocked while trying to acquire the resource.

Some resources are *cumulative*: they hold some number of resource units (work items in a queue, messages on a channel, *et cetera*). The resource record for a cumulative resource will include a list of tasks that are currently blocked on the resource's current state (for example, for a channel, a receiver will block on an empty channel; a sender will block on a bounded channel at full capacity).{% sidenote 'blocking-in-either-direction' 'The fact that a (bounded) channel can cause a sender or receiver to block is an insight that is simultaneously obvious but at the same time raises important modelling questions. In short: the arcs representing blocking dependencies in the task/resource graph may go in *either direction*, depending on the state of the channel. My mind keeps coming back to this point, and I want to make sure we do not forget it.' %}

(In [Holt 1972](#holt72), the terms "reusable" and "consumable" roughly correspond to our usage of "exclusive" and "cumulative". The correspondence is imperfect though; e.g. Holt refers to individual consumable resource units, while for us, a "cumulative resource" describes a collection of such consumable units, such as a channel.)

In addition, the resource record will include lists of tasks that have signaled *intent* to interact with the resource. This way, the resource record for a multi-producer single-consumer channel can include a list of all of the sending tasks that hold the channel, as well as the receiver task associated with the channel, regardless of whether the channel's message buffer is empty, partially-full, or full.

Listing the tasks that intend to interact with the resource is how we can provide the developer with enough information to resolve unexpected blocking behavior in their code. For example, after seeing that a sender task is blocked because a bounded channel is at full capacity, it is simple for the developer to follow hyperlinks to go from the sender task to the channel's resource record, and from there through another hyperlink to the receiver task, and then work on figuring out how to make the receiver task process messages more efficiently.

### Task Records

If a task is blocked, the task record will show what specific `.await` expression it is blocked on, by showing the span (file:line and column range) and the expression itself (`my_channel.send(value).await`). If the blockage is due to a specific resource, then a hyperlink to its resource record will be provided as well.

More generally, a task record shows information about an individual task, including lists of resources associated with the task.

If a task currently holds exclusive access to resources, all such resources will be listed.

As mentioned earlier, tasks can signal intent to interact with a resource. There are two kinds of intention that can be signaled: conditional intent and unconditional intent.{% sidenote 'kinds-of-intent' 'I need to more research to see what related work there is here. My naive notion of "conditional" and "unconditional" intent is probably not novel. What I really want to know is whether it is a *useful* distinction to make.' %}

A task signals *conditional intent* when the interaction is predicated on some external condition holding; i.e., it will occur only on some of the task's non-error control-flow paths. (One could imagine approximating conditional intent by just listing all the resources that are *reachable* from the task; this is a topic we should explore during implementation.)

A task signals *unconditional intent* as a way to state "*if* I can make sufficient forward progress, I *will* interact in this manner with that resource." It is essentially another way of saying: "if you can figure out how to get me unblocked, these are the actions I will take", which can be crucial in resolving certain kinds of resource starvation issues.

The task record shows two separate lists corresponding to the two kinds of intention. This way, developers are informed about *which* tasks to look at first when trying to understand the interactions between tasks and resources in their program.

During execution, resources may move from the conditional list to the unconditional list, or they may move off the conditional list entirely, all according to what a task indicates as its code runs. When a task has actually performed its intended operation to completion (e.g. if it is entirely done sending messages on a given channel, and signals such to the async monitor) then the resource will likewise be removed from the task's resource lists.

> And no one calls us to move on
>
> And no one forces down our eyes
>
> No one speaks and no one tries
>
> No one flies around the sun


### Pausing a Task

Sometimes the information provided in the TurboWish Async Monitor's records will not give enough detail, and the developer will want to inspect the actual state of the task's memory on the live process in a debugger. 

To pause a task, the developer can go to its Task record page, or just select it in the Async Console overview, and then issue the "pause"  command. The executor pauses tasks by opting not to schedule them, instead leaving them suspended where they evaluated `.await`.

As a convenience, the Async Console provides `gdb` and `lldb` commands as helpers that pause the task, then spawn the corresponding debugger processes and attach them appropriately to the paused task.

### Rolling Back the Event Log

Sometimes humans just don't understand how they got into some mess.

For those situations, the Async Monitor offers rollback functionality. Developers can step backwards{% sidenote 'step-backwards' 'I have spent a long time advocating for the use of the [`rr` debugger](https://rr-project.org/), and I suspect this section is a small reflection of that. (This document is describing a completely different operational model to what `rr` does, though. I am emphatically *not* suggesting the event log contain enough state to actually replay program code.)' %} through the event history and see how such steps affect the task and resource records.

For example, a `select!` expression in tokio will run a collection of futures concurrently. It will proceed with the value provided by whichever future completes first, *canceling* the other futures that lost the race. If code is not written to anticipate such cancellation, it can lead to data corruption, or tasks appearing to be blocked indefinitely (aka "hung").

As a concrete example adapted from a [blog post][tomaka blog post], consider this code:

```rust
let mut file = ...;
let mut channel = ...;
loop {
    futures::select! {
        _ => read_send(&mut file, &mut channel) => {},
        some_data => socket.read_packet() => {
            // ...
        }
    }
}
```

Here, `read_send` is constructing a future on every iteration through the loop. If the `socket.read_packet()` invocation ever wins the `select!`-race, then the `read_send` future is dropped, which means the data may have been extracted from the file but not yet relayed on the `channel`. (The details are spelled out on the [blog post][tomaka blog post].)

[tomaka blog post]: https://tomaka.medium.com/a-look-back-at-asynchronous-rust-d54d63934a1c


If a task appears to be hung, the Async Monitor allows the developer to gain insight into why: They can visit the task view for the blocked task. The task view will indicate where it is stuck (in the `select!`). Since `select!` is known to be a source of problems for developers, the view will also include a hyperlink that will "rewind time" to the invocation of `select!` on the previous iteration of this loop, so that one can observe what side-effects it had. Upon rewinding time, the task view indicates that the `socket.read_packet()` future won the race, and that the future returned from `read_send` was *dropped*.

(At this point, we are relying on the developer having an "Aha" moment about the effect of dropping a future when it is in the middle of its computation. It would be good to explore ways to help people get the insight sooner. For example, maybe having the view here indicate *where* the `read_send` future was in its own computation at the time it was dropped would help someone see that we need to keep that same future running.)

#### Constraining event log overhead

As part of the configuration options for the Async Monitor, one can adjust the upper-bound on the size of the event log, in order to ensure its memory usage does not induce memory exhaustion, virtual memory swapping, or other problems that could excessively perturb performance evaluation.

### Concluding the Experience

For many problems, the Async Monitor will provide the developer with the insight they need to resolve their questions about their programs interactions with the async executor. However, some performance problems will require further inspection. The Async Monitor can help with some of that, such as when it allows a developer to [pause a task](#Pausing.a.Task). But sometimes a developer will need insight into other aspects of their program, such as where memory or CPU time is going. Other tools will be needed for such cases.

An appendix of this document shows a diagram of the expected developer [work flow](#Appendix.B:.TurboWish.User.Work.Flow). The diagram presents steps that are described above; but it also shows decision points where a user might need to employ another tool.

Now that we have finished explaining the experience of someone using the tool, we will now discuss how to deliver that experience to our customers.

> Cloudless everyday
>
> You fall upon my waking eyes
>
> Inviting and inciting me to rise


## Implementation Plan

As mentioned above, there are two main components: the Async Monitor, and the Async Console.

The 2021 release of TurboWish will need to provide value without having any support from the Rust compiler or standard library. Therefore, the initial release will work by:
 1. leveraging instrumentation added directly to the async executor,
 2. encouraging service developers to add corresponding instrumentation, and
 3. capturing stack backtraces at key executor events.

### Instrumentation

The instrumentation will be responsible for documenting how the relationships between tasks and resources change over time. More specifically, the instrumentation will capture: task state transitions (running, ready, waiting), task acquisition of exclusive resource (e.g. locking a mutex), task modifications of resource state (e.g. sending on a channel or receiving from it), and task *intent* to interact with resources.

As mentioned in the [Task Records section](#Task.Records), there are two kinds of intent: conditional and unconditional. My long-term hope is to leverage some sort of heap-ownership tracing system to *infer conditional intent*, because signalling it via manual instrumentation will be arduous and error-prone. (Heap ownership tracking alone cannot infer unconditional intent, but it may be possible to leverage compiler analysis to perform such inference of unconditional intent.)

### Component Separation

There are four short-term deliverables: 1. the Async Monitor, 2. the Async Console, 3. a specification of what instrumentation events the Async Monitor understands (which may come from the client code, the async executor, or any related libraries (e.g. Rayon or other crates that offer a thread pool)), and 4. the instrumentation of Tokio (following the aforementioned specification) to support the Async Monitor.


#### Diagram of Async Monitor Architecture

This is a rendering of the component separation. There are two running programs: The program being developed, and the Async Console. Within the program being developed, instrumentation events are generated and travel on the event bus, where they are observed by the Async Monitor running as a thread in that process. The Async Monitor communicates with the Async Console, presenting portions of the model according to the requests that arrive from the console.

<!-- The manual height specification is a rough guess, to allow the rest of the doc to predict the space occupied by the graph once it is rendered. -->
<div class="mermaid" style="height: 750px">
flowchart TD
  subgraph Client
    TwCollect --- pause_reqs ---> Tokio
    pause_reqs([introspection requests,<br/>e.g. 'pause'])
    TracingEventBus
    TracingEventBus -.-> TwCollect
    TwCollect[Async Monitor]
    TwCollect --- EventLog
    EventLog[(EventLog)]

    Tokio[Async Executor<br/>e.g. Tokio]
    ClientCode[Client App Code]
    Rayon

    ClientCode -.-> TracingEventBus
    Tokio -.-> TracingEventBus
    Rayon -.-> TracingEventBus

    TracingEventBus[Event Bus<br/>atop tracing crate<br/>&lt;follows event spec&gt;]
  end

  subgraph Console [Console Program __]
    TwTui <--> TwCollect
    TwTui([Async Console])
  end
</div>



### Development Decoupling

In order to enable concurrent development of the four deliverables, we will follow these steps:

#### Deliverable: Event specification

First, we must pick some format for specifying the grammar of the instrumentation events. Anything with an existing off-the-shelf parser-generator available as a Rust crate will serve. JSON might be a reasonable choice here, at least for initial prototyping; but we should also evaluate whether other event description formats incur less encoding overhead than JSON.

(Another option would be a dedicated enum type for events. That would then need to be exposed in its own crate that downstream clients would need to add as a dependency. ~~Perhaps more importantly: we are very likely to use `tracing` as the event bus, which takes string-encoded messages as the basis for its encoding.~~)

Second, we must make an initial specification of events. Just saying "it's JSON" is not sufficient; we need to state how each desired event is actually encoded in the chosen grammar. This will evolve over time. It must cover everything listed in [Instrumentation](#Instrumentation).

With these two pieces in place, we can spin off a few parallel tracks of development.

#### Deliverable: Instrumenting tokio

For instrumenting tokio. I recommend that we start by developing an event-validating async monitor (or more simply, "event-validator"). The event-validator is responsible for ensuring the incoming event stream is coherent; it might build up a minimal model as part of its coherence check, but it is not responsible for gathering all the metrics that are expected of the Async Monitor product itself. With an event-validator in place, one can test the correctness of tokio instrumentation as it is added. (We will also need to do integration tests once the async monitor itself is constructed.)

#### Deliverable: Async Monitor

For the Async Monitor, I recommend the following baby-steps toward implementation.

First, either make a new mock async executor, or select a pre-existing simple reference async executor (several exist on github), and using this as the mock executor. Use the mock executor for initial development of the Async Monitor: that is, the in-development Async Monitor would be exercised by building an internal model of the mock executor's state. 

The reason for employing a mock executor is two-fold: 1. it will be quicker to add (and modify when inevitably needed) the necessary instrumentation as the Async Monitor itself is developed, and 2. using a mock rather than tokio directly will help prevent inadvertent coupling between tokio itself and the async monitor; such coupling would make it hard to add support to other async executors like async-std in the future.

In addition to a mock executor, I also recommend making an mock event stream that will simulate *both ends* connected to the Async Monitor: it will generate the events that we expect to be signaled by an instrumented application sitting atop some instrumented executor, and it will *also* simulate a console attached to the other side of the Async Monitor. I recommend having a mock event streamer because this is the easiest way to generate a deterministic series of instrumentation events interleaved with console events. Making such event generation deterministic will be crucial for testing the async monitor.

#### Deliverable: Async Console

Finally, for the console, I recommend that we make a mock async monitor. It will simulate the Async Monitor's side of the communication protocol used by any Console connecting to the monitor's port.

(Defining the communication protocol between the Async Monitor and the Console is a prerequisite here. However, this protocol is only exposed to the Monitor and Console, and so it can be revised without having to worry about updating downstream clients.)

#### Deliverable Development Decoupling Diagram

The three decoupled tracks listed above are repeated below in a visual diagram, showing the dependencies between the pieces.

<!-- The manual height specification is a rough guess, to allow the rest of the doc to predict the space occupied by the graph once it is rendered. -->
<div class="mermaid" style="height: 300px">
flowchart TD
    spec[Event Specification]
    spec --> validating_monitor[Event-Validating Async Monitor]
    spec --> mock_executor[Mock Async Executor]
    validating_monitor --> instrument_tokio[Add instrumentation to tokio]
    spec --> mock_event_stream
    mock_event_stream[Mock Event Streamer]
    mock_executor --> async_monitor[Async Monitor]
    mock_event_stream --> async_monitor
    mock_monitor[Mock Async Monitor] --> Console
</div>

### Long-term concerns; short-cuts for the Minimum Viable Product

See [Appendix: Sacrifices for Minimum Viable Product](#Appendix.A:.Sacrifices.for.Minimum.Viable.Product)

> And through the window in the wall
>
> Come streaming in on sunlight wings
>
> A million bright ambassadors of morning


## Metrics

The goal of these tools is to provide users with insight into performance pitfalls. How can we know if the Async Monitor and Console are achieving their goal? 

There are two ways I can imagine approaching this: telemetry from the Async Console program, or evaluating out-of-band signals (such as sentiment analysis on social media). I will focus here on the telemetry option, under the assumption that any potential telemetry would be an opt-in choice presented to the customer when they first use the Async Console tool.

### Telemetry: Success Metrics

For evaluating whether the tool is successfully providing users with insight, the most obvious answer is we could ask our users. When the console detects a problem and the user shifts to the dedicated problem view describing the problem, the console could also proactively ask the "Yes"/"No" question of whether the information it presents is helping the user solve their problem (or perhaps request a user happiness rating on a 1-5 to scale), and then ship those responses back to a service that collects such feedback.

Alternatively: we already plan to have the tool link to websites that provide documentation of various known performance pitfalls. Rather than building telemetry into the console, the linked websites could ask the visitor whether the Async Monitor and Console are working for them. (However, this approach runs the risk of omitting the experiences of users who do not follow the links.)

### Telemetry: Failure Metrics

On the flip side of things, we also want to ensure that the instrumentation from TurboWish is not injecting too much overhead into our customer's applications.

While it is probably not reasonable to try to measure the time spent issuing each individual instrumentation event, it is entirely reasonable to measure how many messages are being sent to the Async Monitor, how large they are, and which component (namely, the aync executor, or user code) issued them.

My recommendation is to monitor how much load the instrumentation is putting onto the event bus, according to the event density over a sliding window of time (let's say 100 ms long). If the event density from user code exceeds some predetermined threshold in any given 100ms window, then the Async Console signals a problem report to the developer, telling them that their program's instrumentation may be slowing down the program. If the event density from the async executor (lets say tokio) exceeds a similar predetermined threshold, then the Async Console reports that up to a service associated with the tokio project. (Or, if the tokio project does not see value in getting such traffic, then the Async Console could suggest that the user file a bug on the tokio github.)

## Security Concerns

The instrumentation added to the tokio runtime and client application code may expose details of internal operation that customers do not exposed to the world at large.

We should check that there are controls in place that ensure either: 1. code deployed to production does not have such instrumentation turned on, or 2. the async monitor is not initiated on production systems,  or 3. the port associated with the async monitor is blocked by a firewall guarding the customer's host machine or intranet.


## Conclusion

The Async Monitor and Console answer a developer's questions about how their code's async executor is behaving as their program runs. They provide a summary of the executor's behavior, with metrics about the tasks and resources their program is using. They allow the developer to identify scheduling decisions and see how task and resources depend on each other.

Furthermore, they serve as the foundation of TurboWish. So, in effect: We aim in 2021 to deliver the foundation for 2022 and beyond. We will work with the Rust community to lay this groundwork, and our community will be enabled to make even more amazing tools atop this foundation.


> And no one sings me lullabies
>
> And no one makes me close my eyes
>
> So I throw the windows wide
>
> And call to you across the sky


# FAQ

 * Q: Why build the Async Monitor into the application binary? Couldn't that be part of the Async Console instead, and have all the events stream to the Async Console?
     * A: The primary reason that the Async Monitor is part of the application binary is that we believe streaming all the events that the monitor needs to observe to an external program will inject too much overhead.
     A secondary reason for building the Async Monitor into the application binary is simplicity. If the monitor were a separate program, then one cannot ensure that the full stream of events reaches the monitor. At best one could strive for a *suffix* of the event stream, which means that the monitor has to be designed to still produce a useful model given such a suffix. (A [previously considered architecture][april 1st iteration] handled the suffix problem by allowing the monitor to pose queries to the instrumented application, in order to get information such as "what is the current set of tasks?")

[april 1st iteration]: https://hackmd.io/QYohB4uTTkas20t6rhCrww

# Appendices

## Appendix A: Sacrifices for Minimum Viable Product

### Minimal Viable Product: Shims for resource instrumentation

<!-- Note (pnkfelix): I'm not sure instrumented replacements will be the actual technique we use. 
 But initial versions of the tool will need to either do this, or something even more invasive, so better to say
 *something* about the need to take this kind of action. -->

*Long-term concern:* We want developers to be able to deploy the Async Monitor with minimal changes to their code. A few lines of initialization code is acceptable, but broad changes to many files is not a good customer experience.

Ways to accomplish this are under active discussion, but any solution with such limited source code modification will require one or more of: 1. changes to the Rust standard library, 2. changes to the rust compiler, or 3. use of low-level features such as [dtrace probes](https://illumos.org/books/dtrace/chp-intro.html) or [eBPF](https://ebpf.io/). We have not yet decided on which of these options is appropriate.

*Short term sacrifice*: In addition to the initialization code described at the start of the description of [the developer experience](#The.Async.Console.Experience), initial versions of TurboWish also require the developer to swap in instrumented versions of common modules; a provided lint will guide developers in this process and help ensure no instances are overlooked. 

```rust
use turbowish::sync::Mutex; // was previously `use std::sync::Mutex;`
```

(As already stated above, longer-term, we hope to leverage other facilities to get these instrumentation events.)

### Minimal Viable Product: Tokio-specific

*Long-term concern:* We want the Async Monitor and Console to work with any popular async executor: tokio and async-std are obvious choices here. For the Async Monitor to be useful on an async program, one must use an async executor that has appropriate TurboWish instrumentation added to it.

*Short term sacrifice:* We will deploy a prototype with tokio, but try to keep in mind any differences with other async executors as we design the protocol used for the async executor to communicate with the async monitor.

### Minimum Viable Product: Require client instrumentation for full-features

*Long-term concern:* Client application code can benefit from adding extra instrumentation to their code. However, developers should be able to use and benefit from TurboWish without going to extremes adding new instrumentation beyond what the executor has out-of-the-box.

Some components of the Async Monitor will work with zero client instrumentation. In particular: the initial console output that shows the list of tasks and how their time is spent between polling, ready, and waiting does not require any client instrumentation.

However, other features of the Async Monitor, such as tasks listing resources with which they *intend* to interact, require either Rust compiler support or client instrumentation, or maybe both.

*Short term sacrifice:* We will prototype under the assumption that clients are willing to add instrumentation. The Async Monitor will differentiate between instrumentation that is "trusted": cases where instrumentation bugs will make the Async Monitor and Console produce misleading results (e.g. if transitions between polling and waiting are omitted or forged), and "untrusted": cases where instrumention bugs, by design of the monitor, will at most lead to confusion, but not outright lies (e.g. if incorrect attributes are attached to a task or resource, the console output showing those attributes will be likewise incorrect, but it need not disrupt other parts of the Async Monitor or Console).


## Appendix B: TurboWish User Work Flow

<!-- The manual height specification is a rough guess, to allow the rest of the doc to predict the space occupied by the graph once it is rendered. -->
<div class="mermaid" style="height: 1750px">
flowchart TD
   Start --> PerfStart
   %% Start --> TasksSeemBlocked
   PerfStart([Performance does not match expectations])
   PerfStart --> QA
   %% TasksSeemBlocked([Tasks seem stuck])
   %% TasksSeemBlocked --> A
   QA{using<br/>async<br/>rust?}
   QA --"yes" --> A
   A --> R --> CC --> QProblemHighlighted
   A[add TurboWish Async Console to service]
   R[start serivce]
   CC[connect to console]
      
   QProblemHighlighted{Console<br/>highlights<br/>problem}
    
   QProblemHighlighted -- "yes" --> ConsoleHighlight
   ConsoleHighlight[observe console output]

   ConsoleHighlight --> PendingWithoutWaker
   PendingWithoutWaker([Console reports:<br/>buggy Future detected])
   
   ConsoleHighlight --> CycleDetected
   CycleDetected([Console reports:<br/>deadlock cycle detected])
   
   PendingWithoutWaker --> UserReadsDocs
   CycleDetected --> UserReadsDocs
   UserReadsDocs[read linked Rust documentation]
   
   UserReadsDocs --> IdentifyRootCause   
   
   QProblemHighlighted -- "no" --> QStuckTask
   
   QStuckTask{any tasks<br/>blocked?}
   
   QStuckTask -- "yes" --> InspectEachStuckTask --> FollowTaskResourceChains --> IdentifyRootCause
   
   InspectEachStuckTask[inspect each stuck task]
   FollowTaskResourceChains[follow task/resource dependencies]
   IdentifyRootCause[identify root cause of problem]
     
   QPH{excessive<br/>memory usage?}
  
   QPH -. "yes" .-> H
   QPH -. "no" .-> P
   
   QA -. "no" .-> QPH
   
   H[use turbowish-heapprof<br/>&lt;future project&gt;]
   P[use turbowish-perf<br/>&lt;future project&gt;]
</div>

## Appendix C: Instrumentation notes (brainstorming)

(This section assumes that a set of related wakers is a reasonable and necessary proxy for "resource". This assumption will be revisited during development; the executor controls creation of wakers, so it is a natural point to instrument; but the wakers may not have sufficient context available at their creation points to support describing their associated resource.) 

The most basic functionality for the task/resource graph user story requires the executor to emit events whenever:

 * a task is spawned,
 * a task is dropped,
 * a waker is created, 
 * a waker is cloned,
 * a waker is dropped, or
 * a waker is transferred from one task to another.

Supporting other user stories will likely require tracking other information as well (such as how many pending futures have had `wake` called and are awaiting a call to `poll`). We may need to add additional hooks to the async executor, analogous to the support for "pause", that the Async Monitor can invoke to turn on tracing of such information.

The emitted events should include unique identifiers (UID) for any referenced task/wakers.

 * For values that are themselves boxed or own a heap-allocated value, we should be able to use a memory address as a UID, as long as we also include some sort of timestamp with the events (and the Event Collector will infer when memory is being reused and update its internal model accordingly).
 * (If we need to track values that do not have a uniquely associated heap values, then we may need to add some sort of unique-id generation for them. So far I haven't seen a need in tokio's types.)

The emitted events should also include some notion of the calling context for the event. This calling context should be meaningful from the viewpoint of the Client App Code.

 * For example, when `<TimerFuture as Future>::poll` calls `cx.waker().clone()`, we want the waker construction event to include (at minimum) that a waker was created from `TimerFuture`, so that one can properly tell what *kind of resource* that `waker` is associated with.
 * (It would be even better to include enough information in the event for the Event Collector to know *which specific resource* is associated with the waker, rather than just its type.

These events may include program-internal details such as (partial) stack traces that will include memory addresses of program code

 * (We cannot change existing APIs to thread through details like file/line-number info in the style of `#[track_caller]`, so in general this is the only way I expect to be able to infer calling context without putting undue burden on client code.)
 * More specifically: Based on my understanding of the API's available, providing contextual info about the calling context of `cx.waker().clone()` will require either 1. client instrumentation that sets some thread- or task-local state, or 2. backtracing through the stack to find instruction addresses that the Async Monitor can, via debuginfo, map back to the calling context.

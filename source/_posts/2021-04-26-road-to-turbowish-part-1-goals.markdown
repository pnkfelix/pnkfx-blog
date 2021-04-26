---
layout: post
title: "Road to TurboWish; Part 1: Goals"
date: 2021-04-26 12:51:35 -0400
comments: false
categories: rust turbowish performance
---
This is a post about some recent history. I want to tell the world about a project I am leading at Amazon Web Services, called "TurboWish."

TurboWish, in a nutshell, is our umbrella term for a planned suite of tools for understanding your Rust program's dynamic behavior. We want the tools to especially focus on providing insights into the *performance* characteristics of your program.

<!-- more -->

Let me take a moment to tell you what has happened so far: How did I get here?

(if you want skip the history/motivations and jump [straight to the doc](#doc:.Opening.Line), you can follow that link; you may find yourself
in another part of the world.)

## Into the blue again

About six months ago, I left Mozilla and joined Amazon Web Services (AWS). I am now part of a new team: the AWS Rust team.

Our team charter is simple: "The AWS Rust team works to make Rust performant, reliable, and productive for all its users."

Details of what that means are spelled out via [our team's tenets](https://aws.amazon.com/blogs/opensource/how-our-aws-rust-team-will-contribute-to-rusts-future-successes/).
One of those tenets, one that is incredibly important to me, is that "we work in the open." This blog post is itself my own attempt to follow through on that promise: I want to be more open about what I've been writing down and circulating amongst a relatively small group of people, and try to be better about putting my draft ideas out into the open from the start going forward.

## Same as it ever was

From my perspective, I still have the same personal career goals: make the Rust programming language an awesome option for developers, by improving the code of the Rust compiler, the design of the Rust language, and the organization of the Rust community.{% sidenote 'not-improve-community' 'Our community is awesome. I would never want to claim that I am trying to "improve" the community itself. But... I also would not claim that we have any silver-bullet when it comes to self-organization.' %}

Those were my goals when I was at Mozilla, and they are still my goals now.

## You may ask yourself

What do performance tools have to do with those goals?

Our team's inspiration for this focus was based on a few observations.

Part of Rust's promise, the reason people are trying Rust out at all, is that it says it can give you the kind of high-performance that in the past was normally the realm of low-level languages like C or C++.{% sidenote 'or-fortran' 'Or FORTRAN, I suppose, though I do not know if Rust is doing much today to target the scientific computing communities that have historically used FORTRAN.' %}

At a customer-obsessed company like Amazon, that promise raises an important question.
Are our team's customers,{% sidenote 'on-customer-term' 'Some people might be confused by this use of the word "customer." I admit confusion too, since I do not expect to get any compensation from Rust developers themselves. But I still like the word here; I like that it challenges preconceptions about what a customer *is*. (Also, I have been using the word "customers" for projects written in Rust, such as Servo, since at least 2016.)' %}
the developers using Rust, actually seeing such performance wins in practice?

## There is water at the bottom of the ocean

Based on some discussions and informal interviews with fellow Rust developers, I saw three groups of people with differing answers to that question.

The first group says "Rust is great! Once I got it compiling and my unit tests passing, the code works and meets my performance expectations." These customers always made me happy; they are usually happy to then point out what pet features they want to see in the future.

The second group says: "I got it compiling, but its not actually as fast as I had hoped. **What should I do now?**"

Okay, to be fair, I'm making a massive over-generalization there. The second group doesn't always say that. Some of them do [extensive analysis](https://blog.mozilla.org/nnethercote/2019/10/11/how-to-speed-up-the-rust-compiler-some-more-in-2019/) to identify awesome ways to make things faster. Some also document exactly what they did and [how you can do it too](http://likebike.com/posts/How_To_Write_Fast_Rust_Code.html).

But the point remains: If Rust *isn't* meeting your performance goals, its not always obvious what to do. Some people with a lot of in-depth experience with compilers or systems analysis have tools in their utility belt that work well for C/C++ and ... they sort of work for Rust.

But those tools do not work as well as I would like, and I'm not sure they are the right answer for the bulk of our community.
I said five years ago that I want Rust to be [Systems Programming for Everybody](https://www.infoq.com/presentations/rust/), and part of that story is that we need tools that everybody can use and understand.

## How do I work this?

The third group says: "I struggle to figure out what design to use. I struggle to get my service compiling. You are asking me what I think about performance, but  I'm debugging deadlocks from my `async` code."

This third group raises an entirely different line of thinking when it comes to tooling. When I first starting thinking about performance monitoring tools, I was thinking solely in terms of gathering and presenting metrics, such as CPU cycles, memory utilization. But this third group represents a broad set of customers for whom such focus is premature: They are hitting road blocks that prevent their project from ever leaving the prototype phase. One big source of problems was `async` code; there's a good chance that `async`-specific tooling will provide the biggest return on investment here.

The concerns of the second and third groups what led us to the TurboWish project: tools that will help our customers make Rust deliver on its promises: [performant, reliable, productive](https://www.rust-lang.org/).

## Letting the days go by

After performing a set of informal interviews with various customers both within and outside of AWS, I felt confident that there **are** real problems here to solve. We want Rust developers to have good tools{% marginnote 'lang-feats' 'Ideally, the tools would leverage features of Rust itself: the type system, the ownership model, the value rendering system provided in `std::fmt`, the virtual method dispatch tables, *et cetera*. But that is not a strict requirement; it is just "nice-to-have."' %}
on hand that will answer their questions.

So, I jumped into writing a goals document, so that our team could explain to other teams at AWS, and ourselves, what we want to make. I shared it with the rest of the AWS Rust team in the middle of February 2021; they had lots of feedback, but I am not including that here (mostly to avoid having to coordinate authorship of this blog post).

I am ending this blog post with that goals document, warts and all, along with meta-commentary in the right-hand margin notes.
In follow-on blog posts this week, I will share some of the next steps that followed after I wrote this.

### doc: Opening Line

{% marginblock %}
You can see in the opening line itself the focus on `async`/`await`, for better or for worse.
{% endmarginblock %}
TurboWish is a framework for profiling Rust programs, focused on illuminating the performance and resource usage of task-oriented code written with async/await.

### doc: Goals

{% marginblock %}
I played a bit of a semantic shell game here, with my use of the term "production code." That term could be interpreted as "code deployed as a live service." Or it could be interpreted as "code compiled in release mode, but still running on development boxes." I plan to talk more about this distinction in later posts, but the short version is: I think we can provide great value today to developers working on their development boxes, without trying to concern ourselves with making a tool that is sufficiently low-overhead and security risk-free that it could be part of a deployed system.
{% endmarginblock %}
*Profile Production Code*: Incorporating the TurboWish Framework is low-overhead: it can be incorporated into production code without producing an undue maintenance burden and without incurring significant performance overhead.

*Domain-specific Feedback*: Frameworks and applications can provide data for specialized metrics, specific to their internal architecture.

*Understand Hidden Costs and Connections*: Frameworks like tokio ease writing asynchronous code because they hide a number of details behind abstractions (such as generator code produced by the Rust compiler, or task queues managed by the tokio runtime). TurboWish exposes those hidden details, allowing developers to correlate them with other program events. It also exposes connections that humans usually have to reconstruct by hand (such as future to resource to future chains that can yield deadlock), allowing one to directly see from Rust’s ownership model how resources are being held in the object graph.

*Framework Agnostic*: Many of Rust’s customers use tokio, but not all of them. async-std  and fuschia_async are other frameworks for asynchronous programming. TurboWish can provide value to any such framework (though it may also provide framework-specific functionality when warranted). For our initial releases, we can focus on tokio alone, but expect integration with others if tokio proves successful.

{% marginblock %}
I included this goal specifically because I had done a bunch of investigation into using [`rr`](https://rr-project.org/), and discovered during that time that some of the cloud development machines hosted on [EC2](https://aws.amazon.com/ec2/) do not support the performance counters that you need for `rr` to function.
{% endmarginblock %}
*EC2 Instance Type Agnostic*: If we make use of any OS specific features (e.g. dtrace probes), they will be available on all EC2 AL2 instances, regardless of instance-type. (Specifically, we cannot require access to CPU performance counters.)

{% marginblock %}
It was an interesting exercise writing this schedule. There are a number of constraints that I was trying to meet that are not represented in this table.
The biggest one was that the Rust release schedule itself follows a six-week cadence; if TurboWish needs any support from `rustc` itself, and we want it to be available in the stable version of the compiler at the end of October, then that means any such support needs to land in the nightly version of `rustc` before July 29th.
{% endmarginblock %}

### doc: Milestones

|Milestone|Deadline|
|-|-|
|3 to 5 User Stories identified for core focus|26 Feb 2021|
|Development Tracks identified|26 Mar 2021|
|PR/FAQ published|2 Apr 2021|
|3 Launch Partners established|30 Apr 2021|
|alpha prototype|2 Jul 2021|
|feedback gathered from demo of alpha to Launch Partners|16 Jul 2021|
|beta release|20 Aug 2021|
|beta integrated with Launch Partner code bases|17 Sep 2021|
|Evaluation Report of beta (interviews with Launch Partners)|24 Sep 2021|
|1.0 release|29 Oct 2021|

### doc: Milestone explanation

_Development Tracks_: Some features of TurboWish will provide the most value to customers if they are developed in concert with additions to other components of the Rust ecosystem, such as the Rust compiler. However, we are not the sole owners of the Rust ecosystem nor those components. We need to identify target components of interest early (and I am assuming that part of that identification will require actual prototyping, which is why I am allocating a month for such prototyping in parallel with drafting the PR/FAQ{% sidenote 'pr-faq' 'I was unaware of the PR/FAQ format before joining AWS, but it is apparently used in many customer-centric and agile development models; see also [this medium post](https://medium.com/agileinsider/press-releases-for-product-managers-everything-you-need-to-know-942485961e31).' %}), so that we can properly prioritize such development. In each case where we do not own the component, we must also establish the backup plan if our desired changes will not land in time for use in the product this year.

_Launch Partners_: Customers within Amazon are willing to evaluate pre-release versions of the product. We should strategically select three such customers to partner with us; these are the Launch Partners. We will give each such launch partner 1.) demonstrations of the alpha proof of concept, 2.) access to the beta minimum viable product, and 3.) dedicated engineering time for integrating the beta into their service. In exchange, the launch partner will give us feedback on the alpha and beta versions of the product (which will inform each subsequent development sprint).

{% marginblock %}
For some reason I was especially proud of the distinction being drawn in this paragraph. I have a somewhat superficial understanding of project management and Agile development methods, so I was not really thinking about whether demo's are common products of a sprint. (And a [post by Johanna Rothman](https://www.jrothman.com/mpd/2007/10/release-able-vs-demo-able/) makes the great point that even a product that is "only" demo-able has still demonstrated *integration amongst the team*.)
From my perspective, the demo/release distinction had an entirely different motivation: I simply did not see time in the schedule for the year for two full release plus integration plus customer-feedback cycles.
{% endmarginblock %}
_Alpha Demo_ versus _Beta Release_: We want to move quickly, develop a minimum viable product and then iterate on it until we have something that delights our customers. We also want to work with a set of dedicated launch partners to evaluate an early version of the product (the beta). However, a product like this is unlikely to be a tool that can be trivially integrated: we expect there to be some amount of development effort associated with linking TurboWish into a given code base. Therefore, we do not expect our launch partners to be able to participate in multiple iterations of evaluating the product, simply due to the amount of development effort each integration is likely to take. So, we propose using different evaluation methodologies for different iteration cycles: For the alpha version, we will integrate TurboWish into code bases that we choose ourselves, give demos of the integrated result to our Launch Partners, and use their feedback in subsequent development of the alpha. For the beta version, we will work with our Launch Partners to integrate TurboWish into their code bases, and then at the end of the integration period, we will use the feedback they provide to make the final changes to the product.

### doc: Risks, Mitigations

> Risk: Time from “development tracks identified” to “PR/FAQ published” is only a week.{% sidenote 'dev-track-to-pr-faq' 'This risk was accurate. It took quite a while longer for me to make a suitable design document. That document will be the subject of a later post.' %}

Mitigation: We need to develop the PR/FAQ in parallel with doing the feasibility studies that identify the development tracks. (But I do not want to make them independent milestones; I want to be confident that the features in the PR/FAQ can be constructed before I try to enlist Launch Partners.)

> Risk: Rust compiler leadership/maintenance will distract pnkfelix.{% sidenote 'compiler-work' 'Another way to look at this: "the TurboWish work will distract pnkfelix from their compiler development efforts." And indeed, both can be true.' %}

Mitigation 1. Get buy-in from others and spread development effort

Mitigation 2. Compiler Team focus for 2021 is rustc contributor experience, especially w.r.t. performance. Hopefully synergies will emerge from that.

> Risk: Not much time remaining in February to establish user stories. Felix’s personal focus for short term are memory usage issues, and so he has been contributing stories related to that. But many customers express concern related to async/await, especially about understanding why their tasks fail to progress (i.e. sources of deadlock).

*(No mitigation documented.)*{% sidenote 'no-mitigation' 'I did not come up with a mitigation for this risk. I just hoped I would have time to write something reasonable.' %}

> Risk: Some features may depend on some amount of GC style tracing, potentially starting from local owned variables on the stack as roots, and in any case traversing values on the heap. (For example, automatically dissecting ownership chains between futures and resources in order to identify causes of deadlock could make use of such tracing.) pnkFelix has experience in this area and believes it to be a solvable problem (especially given the profiling goal, as opposed to correct integration with arbitrary 3rd party GC tech), but it is not a settled problem.

Mitigation: Leverage ownership of relevant frameworks where possible. E.g. you don’t need to trace the local stack if you know your “root set” of interest is the set of tokio tasks. And you don’t need to make an general-purpose value-tracing system when a make-shift trait and associated derive will suffice for the specific problem at hand.

## Time isn't holding us / Time isn't after us

And that's the goals doc, as it stood in mid-February

Like I said above, there was a bit of a journey to get to this point. And even with this document in hand, we do not have enough to start making code: I wouldn't be able to hand this to a programmer and say "do this."

But I had goals. And I had a schedule. What was next on the schedule? *User Stories.* (Those will be the subject of the next blog post.)

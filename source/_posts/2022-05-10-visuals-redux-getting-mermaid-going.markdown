---
layout: post
title: "Visuals redux: Getting mermaid going"
date: 2022-05-10 16:24:52 -0400
comments: false
categories:
mermaid: true
---
This is just a post where I'll either be happy or sad, depending on whether Github pages manages to
render the same content the way that I see it on my local system.

<div class="mermaid">
graph TD

A["Felix tries to add Mermaid support to his blog"]
B["Felix previews blog locally"]
C["Felix tries to deploy blog to github pages"]
A --> B
B --broke--> A
B --works--> C

C--works-->D[Happy]

C--broke-->E(Sad);
</div>

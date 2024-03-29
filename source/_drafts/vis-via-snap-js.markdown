---
layout: post
title: "Vis-a-vis, part 2: Visualizing Graphs via snap.svg"
date: 2015-11-11 18:00
comments: true
categories:
published: true
---

[Previously][part1] I discussed visualizing graphs (`G = (V,E)`)
specified in DOT syntax (`digraph { a -> b; }`) via
`viz.js`, which is `graphviz` compiled to Javascript.

[part1]: /blog/2015/10/12/viz-a-viz-js/

However, I have decided that it is too difficult to convince
graphviz to produce particular layouts. Therefore, I am going
to try out other tools for working with SVG; in this post, I'm
going to discuss a relative newcomer to the scene, [snap.svg][].

<!-- more -->

Control over layout is
particularly important to me, especially when I am showing a
series of graphs and want to ensure that the visual diff between
each one is minimal.

<script src="/javascripts/snap.svg-min.js" charset="utf-8"></script>

<script>
var dev_color = "red";
function grid_pat() {
    var ten = s.path("M 0,0 v10 h10").attr({ fill: "none", stroke: "#aaaaaa", strokeWidth: 1 });
    var hundred = s.path("M 0,0 v 100 h 100 v -100 Z");
    hundred.attr({ stroke: "#555555", strokeWidth: 1 });
    hundred.attr({ fill: ten.pattern(0, 0, 10, 10) });
    return hundred.pattern(0, 0, 100, 100);
}
Snap.plugin(function(Snap,Element,Paper,global) {
    function make_drag_start(f) {
        return function(x,y,evt) {
            var p1 = this.paper.node.createSVGPoint();
            p1.x = evt.clientX;
            p1.y = evt.clientY;
            var screen_to_global = this.paper.node.getScreenCTM().inverse();
            var p3 = p1.matrixTransform(screen_to_global);
            f.call(this, p3.x, p3.y, evt);
        };
    }
    function make_drag_move(f) {
        return function (dx, dy, x, y, evt) {
            var p1 = this.paper.node.createSVGPoint();
            p1.x = evt.clientX;
            p1.y = evt.clientY;
            var screen_to_global = this.paper.node.getScreenCTM().inverse();
            var p3 = p1.matrixTransform(screen_to_global);
            p1.x = dx;
            p1.y = dy;
            var dp = p1.matrixTransform(screen_to_global);
            f.call(this, dp.x, dp.y, p3.x, p3.y, evt);
        };
    }
    function make_drag_end(f) {
        return function(evt) {
            f.call(this, evt);
        };
    }
    Element.prototype.altDrag = function(dm, ds, de, cm, cs, ce) {
        this.drag(make_drag_move(dm), make_drag_start(ds), make_drag_end(de),
                  cm, cs, ce);
        return this;
    };
});
</script>

<script>
// Absolute cartesian coordinates
function A(x, y) {
    var a = [x, y];
    a.is_relative = false;
    a.is_polar = false;
    return a;
}

// Relative cartesian coordinates
function D(dx, dy) {
    var a = [dx, dy];
    a.is_relative = true;
    a.is_polar = false;
    return a;
}

// Relative polar coordinates
function P(dist, theta) {
    var a = [];
    a.is_relative = true;
    a.is_polar = true;
    a.dist = dist;
    a.theta = theta;
    return a;
}

function Node(label, pos) {
    this.is_node = true;
    this.label = label;
    this.pos = pos;
    this.edges = [];
}

function Edge(to, thru) {
    this.is_edge = true;
    this.to = to;
    this.thru = thru;
}

Node.prototype.node = function(label, dpos) {
    var n2;
    if (dpos.is_relative) {
	if (dpos.is_polar) {
	    var dx = dpos.dist * Math.cos(2 * Math.PI * dpos.theta / 360);
	    var dy = dpos.dist * Math.sin(2 * Math.PI * dpos.theta / 360);
	    n2 = [this.pos[0] + dx, this.pos[1] + dy];
	} else {
	    n2 = [this.pos[0] + dpos[0], this.pos[1] + dpos[1]];
	}
    } else {
	n2 = [dpos[0], dpos[1]];
    }
    return new Node(label, n2);
}

Node.prototype.to_node = function(label, dpos, thru) {
    var n = this.node(label, dpos);
    this.edges.push(new Edge(n, thru));
    return n;
}

function node(label, pos) { return new Node(label, pos); }
function edge(to, thru) { return new Edge(to, thru); }
</script>


Goal: render the graph `a -> b -> c -> e -> a; b -> d -> e;` in a nice
way.

<script>
function build_graph() {
    var a = node("a", [100,100]);
    var b = a.to_node("b", D(50,0)); // node("b", [150,100]);
    var c = b.to_node("c", P(50, -45));
    var d = b.to_node("d", P(50,  45));
    var e = d.to_node("e", P(50, -45));

    c.next = edge(e);
    d.next = edge(e);
    e.next = edge(a);

    return [a,b,c,d,e];
}
</script>
    
<svg id="demo_neg16" height="200" width="100%"></svg>
<script>
var s = Snap("#demo_neg16");
var svg = document.getElementById("demo_neg15");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });

(function () {
    g = build_graph();
    for (i in g) {
	if (g[i].is_node) {
	    var n = g[i];
	    var p = n.pos;
	    var rx = n.rx || 20;
	    var ry = n.ry || 10;
	    if (n.label) {
                var t = s.text(p[0], p[1] + 5, n.label);
		t.attr({textAnchor: "middle"});
	    }

	    var c = s.ellipse(p[0], p[1], rx, ry);
	    c.attr({fill: "none", stroke: "blue"});

	    function consider_edge(n, e) {
		if (e.is_edge) {
		    // var l = s.line(p[0], p[1], e.to.pos[0], e.to.pos[1]);
		    var l = s.path(Snap.format(
			("M {start.x},{start.y} " +
			 "C {c1.x},{c1.y} {c2.x},{c2.y} {finis.x},{finis.y}"),
			{start: {x: p[0], y: p[1]},
			 c1: {x: p[0] * 0.66 + e.to.pos[0] * 0.33, y: p[1] * 0.33 + e.to.pos[1] * 0.66},
			 c2: {x: p[0] * 0.33 + e.to.pos[0] * 0.66, y: p[1] * 0.66 + e.to.pos[1] * 0.33},
			 finis: {x: e.to.pos[0], y: e.to.pos[1]}}
		    ));
		    l.attr({fill: "none", stroke: "blue"});
		}
	    }
	    
	    for (j in n) {
		consider_edge(n, n[j]);
	    }
	    for (j in n.edges) {
		consider_edge(n, n.edges[j]);
	    }
	}
    }
})();
</script>

<svg id="demo_neg15" height="200" width="100%">
</svg>
<script>
var s = Snap("#demo_neg15");
var svg = document.getElementById("demo_neg15");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });

var model = {
    start: { x: 10, y:190 },
    ctrl: [{ x:390, y:190 }, { x: 10, y: 10 }],
    finis: { x:210, y: 90 },
};

var e1 = s.ellipse(100, 100, 75, 50)
    .attr({stroke: "green", fill: dev_color, fillOpacity: 0.2});

var e2 = s.ellipse(300, 100, 75, 50)
    .attr({stroke: "purple", fill: dev_color, fillOpacity: 0.2});

var p = s.path().attr({stroke: "blue", fill: "none" });
var cs = s.circle(0,0,5).attr({stroke:"blue", fill:dev_color});
var cf = s.circle(0,0,5).attr({stroke:"blue", fill:dev_color});
var c1 = s.circle(0,0,5).attr({stroke:"orange", fill:dev_color});
var c2 = s.circle(0,0,5).attr({stroke:"orange", fill:dev_color});
var l1 = s.line().attr({stroke: "orange", opacity: 0.5});
var l2 = s.line().attr({stroke: "orange", opacity: 0.5});

var update_shapes = (function() {
    // Make local copies of the instances built above so that
    // other script blocks' choice of variable names won't change
    // the behavior of the callback below.

    // All coordinates zeroed here get filled in by closure below each
    // time it is called (and we call it once immediately after
    // building it); this way has just one point of control for
    // positioning relationships.)
    var my_p = p;
    var my_cs = cs;
    var my_cf = cf;
    var my_c1 = c1;
    var my_c2 = c2;
    var my_l1 = l1;
    var my_l2 = l2;
    var my_model = model;

    function make_path_d() {
        return Snap.format(
            "M {start.x},{start.y} "+
                "C {c1.x},{c1.y} {c2.x},{c2.y} {finis.x},{finis.y}",
            {start:my_model.start, finis:my_model.finis,
             c1:my_model.ctrl[0], c2:my_model.ctrl[1]});
    }

    return function update_shapes() {
        my_cs.attr({cx: my_model.start.x, cy: my_model.start.y});
        my_cf.attr({cx: my_model.finis.x, cy: my_model.finis.y});
        my_c1.attr({cx: my_model.ctrl[0].x, cy: my_model.ctrl[0].y});
        my_c2.attr({cx: my_model.ctrl[1].x, cy: my_model.ctrl[1].y});
        my_l1.attr({x1: my_model.start.x, y1: my_model.start.y,
                    x2: my_model.ctrl[0].x, y2: my_model.ctrl[0].y});
        my_l2.attr({x1: my_model.finis.x, y1: my_model.finis.y,
                    x2: my_model.ctrl[1].x, y2: my_model.ctrl[1].y});
        my_p.attr({d: make_path_d()});
    }
})();

update_shapes();

var make_drag_move = (function() {
    var my_update_shapes = update_shapes;
    return function(model_part) {
        return function(dx, dy, x, y, evt) {
            model_part.x = x; model_part.y = y; my_update_shapes();
        };
    };
})();

cs.altDrag(make_drag_move(model.start), function(x,y,e) {}, function(e) {});
cf.altDrag(make_drag_move(model.finis), function(x,y,e) {}, function(e) {});
c1.altDrag(make_drag_move(model.ctrl[0]), function(x,y,e2) {}, function(e) {});
c2.altDrag(make_drag_move(model.ctrl[1]), function(x,y,e) {}, function(e) {});

function make_ellipse_draggable(e) {
    var my_e = e;
    var dcx;
    var dcy;
    e.altDrag(function(dx, dy, x, y, evt) { true;// console.log("got here x:"+x+" y:"+y);
					    my_e.attr({cx:x - dcx, cy:y - dcy});
					  },
	      function(x, y, evt) {
		  dcx = x - my_e.attr("cx");
		  dcy = y - my_e.attr("cy");
	      },
	      function(evt) { });
}
make_ellipse_draggable(e1);
make_ellipse_draggable(e2);
</script>

<svg id="demo_neg14" height="100" width="100%"/>
<script>
var s = Snap("#demo_neg14");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var desc = {x: 10, y: 10,
            ctrl: { x: 80, y: 30 },
            tgt: { x: 40, y: 70 },
            rest: [ 30,60, 50,60, 70,60 ]}
s.path(Snap.format("M{x},{y} Q {ctrl.x},{ctrl.y} {tgt.x},{tgt.y} T {rest}",
                   desc))
    .attr({stroke: "black", fill: "none" });
s.group(s.circle(desc.x, desc.y, 5),
        s.circle(desc.ctrl.x, desc.ctrl.y, 5),
        s.circle(desc.tgt.x, desc.tgt.y, 5))
    .attr({stroke: "darkorange", fill: "none"});
for (var i=0; i < desc.rest.length; i += 2) {
    s.circle(desc.rest[i], desc.rest[i+1], 5)
        .attr({stroke: "red", fill: "none"});
}
</script>

<svg id="demo_neg13" height="200" width="100%"/>
<script>
var s = Snap("#demo_neg13");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var tm = s.text(0,60, "Hello World").attr({fontFamily: "monospace", fontSize: "40pt", textLength: 100, stroke: "blue", fill: "gray" });
var tm2 = tm.clone().attr({fill: s.path("M 0,0 0,10 10,10 10 0 z").attr({stroke:"orange", strokeWidth: 3}).pattern(0,0,10,10) });
var br = s.rect(5,10,90,80).attr({fill: "gray"});
s.group(br.clone(), tm2.clone()).transform("translate(0,100)");
// var e = s.ellipse(50, 45, 55, 35).attr({stroke:"orange"}).attr({fill:"gray"});
// e.attr({mask: tm.clone().attr({ fill: "white" })});
// var g = s.group(e,
//                 s.rect(0, 0, "100%", "100%").attr({opacity: 0.5}));
// g.clear();
var p = s.path("M 90,10 Q 20,20 30,10 T 30,50 T 10,50 T 50,40 Q 20,40 50,70 T 60,70 T 70,50 T 70,30 T 90,70 z").attr({stroke:"blue",fill:"red",fillRule:"evenodd"});
var p2 = p.clone().attr({fill: s.gradient("l(0,0,1,1)red-white-blue-white-red-white-blue")});
p2.clone().transform("translate(100,100)");
s.group(br, tm2).attr({mask:p2}).transform("translate(200,100)");
var t = s.text(10, 50, "Hello World").attr({fontFamily: "monospace", fontSize:"15pt", textLength: 80});
p.clone().transform("translate(100,0)");
s.group(tm.clone(), t.clone()).attr({mask: p.attr({fill: "white"})}).transform("translate(200,0)");
</script>

<svg id="demo_neg12" height="200" width="100%"/>
<script>
var s = Snap("#demo_neg12");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });

function arc_p(large, sweep) {
    return "M 110,70 A 50 330 20 "+large+" "+sweep+" 160 110"
}
s.path("M 110,10 Q 20,20 30,10 T 110,70").attr({ stroke: "blue", fill: "none" });
s.path(arc_p(0,0)).attr({ stroke: "#f00", fill: "none" });
s.path(arc_p(0,1)).attr({ stroke: "#fb0", fill: "none" });
s.path(arc_p(1,0)).attr({ stroke: "#0cf", fill: "none" });
s.path(arc_p(1,1)).attr({ stroke: "#c0f", fill: "none" });
s.path("M 160,110 q 20,20 30,10 t 50,70").attr({ stroke: "blue", fill: "none" });
s.text(0,0,"clockwise, big: 1 1") .attr({fill:"#c0f", fontSize:"6pt",textpath: "M 110 70 m 10 -20 A 50 30 20 1 1 160 110" }).transform("translate(2, -2)");
s.text(0,0,"clockwise, lil': 0 1").attr({fill:"#fb0", fontSize:"6pt",textpath: "M 110 70 m 3 0 A 50 30 20 0 1 160 110" }).transform("translate(2, -2)");
s.text(0,0,"ccw, lil': 0 0")      .attr({fill:"#f00", fontSize:"6pt",textpath: "M 110 70 m 1 10 A 48 29 20 0 0 160 110" }).transform("translate(3, -1)");
var p = "M  70 110 A 51 29 20 0 0 160 110";
var xform = "translate(-1, -5)";
// s.path(p).attr({fill:"none", stroke:"green"}).transform(xform);
s.text(0,0,"counterclockwise, big: 1 0")       .attr({fill:"#0cf", fontSize:"6pt",textpath: p }).transform(xform);
</script>

<svg id="demo_neg11" height="200" width="100%">
<rect x="60%" y="0" height="100%" width="10%" fill="url(#barber)"/>
<linearGradient id="gradient1" >
    <stop offset="0" stop-color="black"/>
    <stop offset="0.3" stop-color="white" stop-opacity="0"/>
    <stop offset="0.4" stop-color="white" stop-opacity=".8"/>
    <stop offset="0.6" stop-color="white" stop-opacity=".0"/>
    <stop offset="1" stop-color="black"/>
</linearGradient>
</svg>
<script>
var s = Snap("#demo_neg11");
s.rect(0, 0, "100%", "100%").attr({ fill: "grey" });
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var base = s.g(s.rect(0,  0, "100%", 40).attr({fill: "#e12"}),
               s.rect(0, 40, "100%", 35).attr({fill: "#ddd"}),
               s.rect(0, 75, "100%", 40).attr({fill: "#11c"}),
               s.rect(0,115, "100%", 35).attr({fill: "#ddd"})
               ,s.circle(120,  22, 2).attr({fill: "brown"})
               ,s.circle(140,  72, 1).attr({fill: "black"})
               ,s.circle( 60, 122, 1).attr({fill: "brown"})
               ,s.ellipse( 80,  32, .5, 17).attr({fill: "#282"})
              );
var bshop = s.g(base
                ,base.use().transform("translate(0, -150)")
                ,base.use().transform("translate(0,  150)")
                ,base.use().transform("translate(0, -300)")
               )
    .transform("skewY(45)");
var axf = document.createElementNS("http://www.w3.org/2000/svg", "animateTransform");
axf.setAttribute("attributeName", "transform");
axf.setAttribute("type", "translate");
axf.setAttribute("dur", "2s");
axf.setAttribute("from", "0 0");
axf.setAttribute("to", "-150 0");
axf.setAttribute("repeatCount", "indefinite");
var barber_still = bshop.use().transform("translate(0, -50)");
var barber = barber_still
    .append(axf)
    .toPattern(0, 0, "30%", 150);
s.rect("10%", "0", "10%", "100%").attr({ fill: barber });
s.rect("30%", "0", "10%", "100%").attr({ fill: s.select("#gradient1") });
s.rect("50%", "0", "10%", "100%").attr({ fill: barber });
s.rect("50%", "0", "10%", "100%").attr({ fill: s.select("#gradient1") });
// base.toDefs();
bshop.toDefs();
barber.toDefs();
</script>

<svg id="demo_neg10" height="300" width="100%">
  <defs>
    <linearGradient id="gradient1">
      <stop offset="0" stop-color="black"/>
      <stop offset="0.3" stop-color="white" stop-opacity="0"/>
      <stop offset="0.4" stop-color="white" stop-opacity=".8"/>
      <stop offset="0.6" stop-color="white" stop-opacity=".0"/>
      <stop offset="1" stop-color="black"/>
    </linearGradient>
  </defs>
</svg>
<script>
var s = Snap("#demo_neg10");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var cylinder = s.gradient(
    ("l(0,1,0,0)"+
     "red-white-white-blue-blue-white-white-red"
     +"-red-white-white-blue-blue-white-white-red"
     +"-red-white-white-blue"
    ));
cylinder.attr({ gradientTransform: "rotate(15,0.5,0.5)" });
s.rect(10, 10, 110, 250).attr({ fill: cylinder });
s.rect(10, 10, 110, 250).attr({ fill: s.select("#gradient1")});
</script>

<svg id="demo_neg9" height="200" width="100%"/>
<script>
var s = Snap("#demo_neg9");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var t = s.text(
    10, 50,
    "Text along a curve is much easier than placing letters"+
        " one by one");
t.attr({ fontFamily: "monospace", fontSize: "16.55px" });
var points = ["M", [ 50,150],
              "Q", [170,170], [150, 100],
              "T", [200, 30],
              "T", [300,100],
              "T", [350,150],
              "T", [400,100],
              "T", [500,100],
              "T", [600,100],
         ];
var grouped = [];
var path = "";
for (i in points) {
    path += " " + points[i];
    if (Snap.is(points[i], "array")) {
    grouped.push(s.circle(points[i][0], points[i][1], 5));
    }
}
var line_path =
    "M 50,150 L 170,170 150,100 130,30 200,30 270,30 300,100 330,170 "+
    " 350,150 370,130 400,100"+
    " 430,70 500,100 570,130 600,100 630,70";
s.path(line_path)
    .attr({ fill: "none", stroke: "orange", strokeDasharray: "10 2", opacity: 0.5 });
s.group.apply(s, grouped)
    .attr({ fill: "none", stroke: "orange", strokeDasharray: "10 2", opacity: 0.5 });
s.group(s.circle(130,  30, 5),
     s.circle(270,  30, 5),
    s.circle(330, 170, 5),
    s.circle(370, 130, 5),
    s.circle(430,  70, 5),
    s.circle(570, 130, 5),
    s.circle(630,  70, 5)
       ).attr({ fill: "none", stroke: "blue", strokeDasharray: "1 1", opacity: 0.5 });
console.log("path: "+path);
s.path(path).attr({ fill: "none", stroke: "green", opacity: 0.25 });
t.attr({textpath: path });
</script>

<svg id="demo_neg8" height="101" width="100%"/>
<script>
var s = Snap("#demo_neg8");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var t = s.text(
    10, 50,
    ["H", "e", "l", "l", "o", " ", "W", "o", "r", "l", "d",
     " bunch of tspans so many but does it line up zoom out and see"]
).attr({ fontFamily: "monospace", fontSize: "16.55px" });
t.selectAll("tspan")[2].attr({  y: 55 });
t.selectAll("tspan")[6].attr({  y: 50 });
t.selectAll("tspan")[7].attr({ dx:  5 });
t.selectAll("tspan")[8].attr({ dx: -5, dy: -5 });
var t = s.text(
    10, 90, ("Instead adjust each letter if positioning matters; "+
         "then font width irrelevant").split('')
).attr({ fontFamily: "monospace", fontSize: "16.55px" });
var chars = t.selectAll("tspan");
chars[2].attr({  y: 95 });
chars[6].attr({  y: 90 });
chars[7].attr({ dx:  5 });
chars[8].attr({ dx: -5, dy: -5 });
for (var i = 0; i < chars.length; i++) {
    if (i < 10) { continue; }
    // console.log("chars[i].attr i:"+i);
    chars[i].attr({ x: 10 + i*10 });
}
</script>

<svg id="demo_neg7" height="101" width="100%"/>
<script>
var s = Snap("#demo_neg7");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var t = s.text(0,0,"Hello World").attr({ fontFamily: "monospace" });
t.use().attr({ x:  10, y:  90, fontSize: "20px" });
t.use().attr({ x: 110, y:  50, fontSize: "10px", fill: "red" });
t.use().attr({ x: 210, y:  90, fontSize: "20pt", fill: "blue" });
t.use().attr({ x: 210, y:  50, fontSize: "10pt", fill: "green" });
t.toDefs();
</script>

<svg id="demo_neg6" height="200" width="100%"/>
<script>
var s = Snap("#demo_neg6");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });

s.group(
    s.group(s.circle( 70, 140, 5),
        s.line(70, 140, 150, 10),
        s.circle(150,   10, 5),
        s.line(150,  10, 200, 100),
        s.circle(200, 100, 5),
        s.line(200, 100, 250, 190),
        s.line(250, 190, 230, 100),
        s.circle(230, 100, 5),
        s.line(230, 100, 210,  10),
        s.line(210,  10, 250,  70),
        s.circle(250,  70, 5))
    .attr({ fill: "none", stroke: "orange", strokeDasharray: "10 2" }),
    s.group(s.circle(210,  10, 5),
        s.circle(250, 190, 5))
    .attr({ stroke: "blue", fill: "none", strokeDasharray: "1 1" }),
    s.path("M 70,140 Q          150,10 200,100 T 230,100 T 250,70"+
       "M 135,75 L 165,75 165,95 135,95 z")
    .attr({ fill: "red", stroke: "green", fillRule: "evenodd", fillOpacity: 0.2 }))
    .attr({ transform: "translate(-30,   0) scale(0.5)" });

s.group(
    s.group(s.circle( 70, 140, 5),
        s.line( 70, 140, 100,  10),
        s.circle(100,  10, 5),
        s.circle(150,  10, 5),
        s.line(150,  10, 200, 100),
        s.circle(200, 100, 5),
        s.line(200, 100, 250, 190),
        s.circle(200, 150, 5),
        s.line(200, 150, 250, 100),
        s.circle(250, 100, 5),
        s.line(250, 100, 300,  50),
        s.circle(200,  30, 5),
        s.line(200,  30, 270, 100),
        s.circle(270, 100, 5))
    .attr({ fill: "none", stroke: "orange", strokeDasharray: "10 2" }),
    // s.circle(210,  10, 5).attr({ stroke: "blue", fill: "none" }),
    s.group(s.circle(250, 190, 5),
        s.circle(300,  50, 5))
    .attr({ stroke: "blue", fill: "none", strokeDasharray: "1 1" }),
    s.path("M 70,140 C 100,10   150,10 200,100 S 200,150 250,100 S 200,30 270,100"+
       "M 135,75 L 165,75 165,95 135,95 z").
    attr({ fill: "red", stroke: "purple", fillRule: "evenodd", fillOpacity: 0.2 }))
    .attr({ transform: "translate(70,   0) scale(0.5)" });
</script>


<svg id="demo_neg5" height="200" width="100%"/>
<script>
var s = Snap("#demo_neg5");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
s.group(
    s.path("M 70,140 L          150,0 200,100 L         40,100 100,0 L         170,140 70,140").
    attr({ id: "H",  fill: "#bbb", fillRule: "evenodd" }),
    s.path("M 70,140 Q          150,0 200,100 Q         40,100 100,0 Q         170,140 70,140").
    attr({ id: "X",  fill: "#b42", fillRule: "evenodd", stroke: "green" }),
    s.path("M 70,140 C 17.5,140 150,0 200,100 C 220,140 40,100 100,0 C 127,-47 170,140 70,140").
    attr({ id: "X2", fill: "#c53", fillRule: "evenodd", stroke: "blue", opacity: 0.5 })
).attr({ transform: "translate(0, 25)" });
</script>

<svg id="demo_neg4" height="101" width="100%"/>
<script>
var s = Snap("#demo_neg4");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
s.path("M 20,40 Q 40,80 60,40").
    attr({ fill: "none", stroke: "blue" });
s.path("M 20,40 L 40,80 60,40").
    attr({ fill: "none", stroke: "red" });

</script>    

<svg id="demo_neg3" height="101" width="100%"/>
<script>
var s = Snap("#demo_neg3");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var c = s.path("M 10,35 30,10 50,35 z " +
           "M 25,32 25,22 35,22 35,32 z");
c.attr({ fill: "#ff8", stroke: "black", strokeWidth: 1.5, fillRule: "evenodd" });
</script>

<svg id="demo_neg2" height="101" width="100%"/>
<script>
var s = Snap("#demo_neg2");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var c = s.path("M 7,29 L 15,15 20,25 4,25 10,15 17,29");
var c = s.path("M 7,29 L 15,15 20,25 4,25 10,15 17,29");
c.attr({ fillRule: "evenodd", transform: "translate(25,0) " });
</script>

<svg id="demo_neg1" height="101" width="100%"/>
<script>
var s = Snap("#demo_neg1");
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });
var c = s.path("M 5 5 L 15 15");
c.attr({ stroke: "black" });
var c = s.path("M 15 5 L 25 15 35 10");
</script>

<svg id="demo0" height="800" width="100%"/>
<script>
// Adapted from examples in "Building Web Apps with SVG"
var s = Snap("#demo0");
// var grid = s.path("M10-5-10,15 M15,0,0,15 M0-5-20,15").attr({ fill: "none", stroke: "#bada55", strokeWidth: 5 }).pattern(0, 0, 10, 10);
s.rect(0, 0, "100%", "100%").attr({ fill: grid_pat() });

var rects = s.path("M 3,3 3,8, 8,8 8,3 3,3").attr({ fill: "none", stroke: "seagreen", strokeWidth: 2 }).pattern(0, 0, 10, 10);

// line(x1, y1, x2, y2);
var l = s.line(0, 0, 100, 100);
l.attr({ stroke: rects, "stroke-width": 10, "stroke-linecap": "round" });
// rect(x, y, width, height, [rx], [ry])
var r = s.rect(50, 50, 300, 170, 90, 50);
var shape_attrs = { stroke: rects,
            "stroke-width": 10,
            fill: "lightgray",
            "fill-opacity": 0.6 };
r.attr(shape_attrs);
// circle(cx, cy, r)
var c = s.circle(150, 150, 100);
c.attr(shape_attrs);
// ellipse(cx, cy, rx, ry);
var e = s.ellipse(110, 55, 70, 35);
e.attr(shape_attrs);
// polyline(points-array)
var pl = s.polyline([200,60, 240,230, 310,230, 350,60]);
pl.attr({ fill: "lightcyan", "fill-opacity": 0.5, stroke: "darkviolet", "stroke-width": 25, "stroke-linecap": "round", "stroke-opacity": 0.2 });
// polygon(points-array)
var pg = s.polygon([100,50, 115,120, 150,150, 115,180, 100,250, 85,180, 50,150, 85,120]);
pg.attr({ fill: "darkorange", "fill-opacity": 0.5, stroke: "papayawhip", "stroke-width": "25", "stroke-opacity": 0.7, "stroke-linejoin": "miter" });

// A series of lines illustrating the flexibility of what one can do via
// a single line by varying its width, opacity, dasharray, and linecap.
(function () {
    var lines = [];
    function make_line() {
    var l = s.line(15, 30, 152, 40);
    l.attr({ stroke: "darkslategray" });
    lines.push(l);
    return l;
    }
    make_line().attr({strokeWidth: 40, strokeDasharray: "2,18" });
    make_line().attr({strokeWidth: 20, strokeOpacity: "0.5", strokeDasharray: "20,20" });
    make_line().attr({strokeWidth: 40, strokeLinecap: "round" });
    make_line().attr({strokeWidth: 10, strokeDasharray: "0,20", strokeLinecap: "round" });
    make_line().attr({strokeWidth: 40, strokeOpacity: "0.5", strokeLinecap: "round" });
    make_line().attr({
    strokeWidth: 30,
    strokeDasharray: ("1,1,1,1,1,2,2,2,2,1,1,1,4,1,1,2,2,2,1,1,1,"+
              "1,1,1,1,1,2,3,1,1,2,1,3,1,1,3,1,2,1,2,1,1") });

    for (i in lines) {
    s.g(lines[i]).attr({ transform: "translate( 0, " + i * 40 + ")" });
    }
    return lines;
})();

// A series of circles illustrating the flexibility of what one can do via
// a single circle by varying its width, opacity, dasharray, and linecap.
(function() {
    var circles = [];
    function make_circle(cx, cy, r) {
    var c = s.circle(cx, cy, r);
    c.attr({ stroke: "darkslategray",
         fill: "lightslategray",
         fillOpacity: 0.5 });
    circles.push(c);
    return c;
    }
    make_circle( 40, 40,20).attr({strokeWidth: 20,strokeOpacity: "0.5" });
    make_circle(120, 40,20).attr({strokeWidth: 10,strokeDasharray: "1.75 8.72"})
    make_circle( 40,100,10).attr({strokeWidth: 20,
                  strokeDasharray: "15.708 15.708",
                  strokeDashoffset: "7.854"});
    make_circle(120,100,10).attr({strokeWidth: 20,
                  strokeDasharray: "47.124 15.708",
                  strokeDashoffset: "7.854"});
    make_circle( 40,160,20).attr({strokeWidth: 2,
                  strokeDasharray: "31.416 31.416",
                  strokeDashoffset: "-15.708"});


    var c1 = s.circle(330, 40, 20);
    c1.attr({ stroke: "darkslategray",
         fill: "lightslategray",
         fillOpacity: 0.5 });
    c1.attr({strokeWidth: 10,
         strokeDasharray: "1.75 8.72",
         fillOpacity: "0.5"});
    var c2 = s.circle(330, 40, 10);
    c2.attr({ stroke: "darkslategray",
         fill: "lightslategray",
         fillOpacity: 0.5 });
    c2.attr({strokeWidth: 20,
         strokeDasharray: "0.875 14.833 0.875 100",
         fill: "none"});
    circles.push(s.g(c1, c2));

    for (i in circles) {
    s.g(circles[i]).attr({ transform: "translate( 200, " + i * 40 + ")" });
    }
    return circles;

})();


s.g(l).attr( { transform: "translate(  0,0)" });
s.g(r).attr( { transform: "translate(100,0)" });
s.g(c).attr( { transform: "translate(  0,200)" });
s.g(e).attr( { transform: "translate(300,200)" });
s.g(pl).attr({ transform: "translate(  0,500)" });
s.g(pg).attr({ transform: "translate(  0,500)" });



</script>

SVG Presentation attributes:

  * `stroke` : color of the stroke; color string (named, hexadecimal, RGB, HSL. see also [w3](http://www.w3.org/TR/SVG/color.html)).

  * `stroke-width` : width of stroke for shape/text ; percentage or length (with optional unit).

  * `stroke-opacity` : transparency of stroke; 1.0 (opaque) to 0.0 (invisible).

  * `stroke-dasharray` : spacing to use between line segments of stroke; list of user coordinate values (px).

  * `stroke-linecap` : shape at both end of line; "butt", "round", "square"

  * `stroke-line-join` : shape at corners of path/shape; "miter", "round", "bevel"

  * `fill` : color of shape or text; color string.

  * `fill-opacity` : transparency of fill; 1.0 (opaque) to 0.0 (invisible). Note that it can mix with a tranparent stroke.

  * `fill-rule` : portions of shape to be filed (e.g. when its path is self-intersecting); "nonzero", "evenodd"

(TODO: Almost certainly remove all of this before publishing!)
<svg id="demo_getting_started" height="300" width="300"/>

<svg width="0" height="0">
  <pattern id="pattern" patternUnits="userSpaceOnUse" x="0" y="0" width="10" height="10" viewBox="0 0 10 10">
    <path d="M-5,0,10,15M0-5,15,10" stroke="white" stroke-width="5"/>
  </pattern>
</svg>

<script>
// taken from http://snapsvg.io/start/
var s = Snap("#demo_getting_started");
var bigCircle = s.circle(150, 150, 100);
bigCircle.attr({
    fill: "#bada55",
    stroke: "#000",
    strokeWidth: 5,
});
var smallCircle = s.circle(100, 150, 70);
var discs = s.group(smallCircle, s.circle(200, 150, 70));
discs.attr({
    fill: "#fff"
});
bigCircle.attr({
    mask: discs
});
smallCircle.animate({r: 50}, 10000);
discs.select("circle:nth-child(2)").animate({r: 50}, 10000);

// Q: What is the "x-y-z" syntax for the Moves below?
var p = s.path("M10-5-10,15 M15,0,0,15 M0-5-20,15").attr({
    fill: "none",
    stroke: "#bada55",
    strokeWidth: 5
});

p = p.pattern(0, 0, 10, 10);
bigCircle.attr({
    fill: p
});
discs.attr({
    fill: Snap("#pattern")
});
// Lowercase "r" is relative radial graident. (Relative for each circle)
discs.attr({fill: "r()#fff-#000"});
// Uppercase "R" is absolute graident for whole group
discs.attr({fill: "R(150, 150, 100)#fff-#000"});

p.select("path").animate({stroke: "#f00"}, 10000);
bigCircle.drag();
s.text(200, 100, "Snap.svg");
var t = s.text(200, 120, ["Snap", ".", "svg"]);
t.selectAll("tspan:nth-child(3)").attr({
    fill: "#900",
    "font-size": "20px",
});
</script>

[Previously][part1] I discussed visualizing graphs (`G = (V,E)`)
specified in DOT syntax (`digraph { a -> b; }`) via
`viz.js`, which is `graphviz` compiled to Javascript.

[part1]: /blog/2015/10/12/viz-a-viz-js/

However, this technique can be quite frustrating, since most of the
graphviz layout engines provide very little control over placement of
nodes and routing of edges. Usually if one wants to constrain the
placement of any one or more nodes, one is forced to specify the
location of *all* nodes -- and even then, the edge routing is nearly
uncontrollable.

Therefore, I want to explore using SVG (scalable vector graphics) more
directly, but still using some Javascript library to ease encoding the
`<svg>` elements.

### Installing JS for snap.svg

<script src="/javascripts/snap.svg-min.js" charset="utf-8"></script>

[snap.svg]: http://snapsvg.io/

### Demos

<svg id="demo1" height="500" width="300"/>

<script>
var s = Snap("#demo1");
// var s = Snap(500, 300);
var bigCircle = s.circle(150, 150, 100);
</script>

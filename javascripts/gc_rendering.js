function digraph(content, props) {
     var wrapped = 'digraph { bgcolor="transparent"; ' +
      ' overlap="false"; ' +  // if left out, nodes may overlap
      ' start=0; ';           // seed the RNG (for consistency)
     for (k in props) {
       wrapped += '\n' + k + '= \"' + props[k] +'\";';
     }
     wrapped +=
      '\n' + content  + '}';
     return wrapped;
}

function lr_digraph(content, props) {
    return digraph('rankdir="LR"; '+content, props);
}

function make_regfile(rf_id) {
    var rf = {
        id: rf_id,
        label: "<id>" + rf_id + " | <r0>r0 | <r1>r1 | <r2>r2 | <r3>r3",
        shape: "record"
    };

    rf.link = function(source, target, options) {
        if (!(0 <= source <= 3)) {
            console.error("unknown regfile source: "+source);
        }
        var edge = {
            source_port: ':r'+source+':e',
            target: target,
            is_edge: true,
        };
        if (options) {
            for (k in options) {
                edge[k] = options[k];
            }
        }
        this['r'+source] = edge;
    }

    return rf;
}

// Warning: I have not yet managed to get `options` to work.
function post_graph(target, g, options) {
    var elem = document.getElementById(target)
    // elem.innerHTML += "<code>" + g + "</code>"
    if (options) {
        elem.innerHTML += Viz(g, options);
    } else {
        elem.innerHTML += Viz(g, "svg");
    }
}

function post_objects(target, objects, options) {
    var with_code = options && options.with_code;
    var g = digraph(render_objects(objects), options);
    var elem = document.getElementById(target);
    if (with_code) { elem.innerHTML += "<pre>" + g + "</pre>"; }
    elem.innerHTML += Viz(g, "svg")
}

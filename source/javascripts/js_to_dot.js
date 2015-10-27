// -*- mode: javascript; indent-tabs-mode: nil -*-

// Note: (May initialize `object.shape`)
function label(object, options) {
    // (may generalize to support attached methods later.)
    var lbl = object.label;
    if (options && options.show_node_records && !lbl && !object.shape) {
        lbl = "<id>" + object.id;
        for (idx in object) {
            if (idx == "id") { continue; }
            lbl += " | ";
            lbl += "<" + idx + ">";
            lbl += idx;
        }
        object.shape = "record";
    }
    if (!lbl) {
       lbl = object.id;
    }
    return lbl;
}

// A node object has non-false .id property.

// An edge object has non-false .is_edge property.

// A subgraph object has non-false .is_subgraph property. (It can
// also carry an optional non-false .id property.)

function is_node(object) { return object.id && !object.is_subgraph; }
function is_edge(object) { return object.is_edge; }
function is_subgraph(object) { return object.is_subgraph; }
function is_graphviz(object) { return is_node(object) || is_edge(object) || is_subgraph(object); }

function render_node(node, options) {
    var content = "";
    content += node.id;
    var lbl = label(node, options);
    content += '[';
    if (lbl) {
        content += "label=\"" + lbl + "\",";
    }

    for (idx in node) {
        if (node[idx].id) { continue; }
        if (idx == "label") { continue; }
        if (idx == "id") { continue; }
        if ((typeof node[idx]) == "function") { continue; }
        var val = ""+node[idx];
        if (val == "[object Object]") { continue; }
        content += idx + "=\"" + val + "\",";
    }

    content += ']; ';
    return content;
}

function render_edge(object_a, object_b, edge_data, options) {
    var content = "";
    var source_port = "";
    if (edge_data.source_port) {
        source_port = edge_data.source_port;
    }
    var target_port = "";
    if (edge_data.target_port) {
        target_port = edge_data.target_port;
    }
    content += object_a.id + source_port +
               ' -> ' +
               object_b.id + target_port;
    var wrote_bracket = false;
    for (idx in edge_data) {
        if (idx == "is_edge") { continue; }
        var val = ""+edge_data[idx];
        if (val == "[object Object]") { continue; }
        if (!wrote_bracket) {
            content += "[";
            wrote_bracket = true;
        }
        content += idx + "=\"" + val + "\",";
    }
    if (wrote_bracket) { content += "]"; }
    content += "; ";
    return content;
}

function render_subgraph(subgraph, options, indent, seen_ids, todo) {
    var content = "";
    var subgraph_id = subgraph.id;
    if (subgraph_id) {
        content += "\n" + indent + "subgraph " + subgraph_id;
        seen_ids.push(subgraph_id);
    }
    content += "{ ";
    for (idx in subgraph) {
        var object = subgraph[idx];
        if (!is_graphviz(object)) {
            if (idx == "id") { continue; }
            if (idx == "is_subgraph") { continue; }
            if ((typeof object) == "function") { continue; }
            var val = ""+object;
            if (val == "[object Object]") { continue; }
            content += idx + "=\"" + val + "\";";
        }
    }
    for (idx in subgraph) {
        var object = subgraph[idx];
        if (!is_graphviz(object)) {
            continue;
        } else if (is_node(object)) {
            var node = object;
            content += "\n    " + render_node(node, options);
            if (seen_ids.indexOf(node.id) == -1) {
                seen_ids.push(node.id);
                todo.push(node);
            }
        } else if (is_subgraph(object)) {
            content += render_subgraph(object, options, indent + "    ", seen_ids, todo);
        }
    }
    content += "}";

    return content;
}

function render_objects(objects, options) {
    var subgraphs = [];
    (function () {
        function on_subgraph(subgraph) { subgraphs.push(subgraph); }
        for_each_reachable(objects, { on_subgraph: on_subgraph, pure_traversal: true });
    })();

    var content = "";
    var seen_ids = [];
    var todo = [];

    for (idx in subgraphs) {
        var subgraph = subgraphs[idx];
        if (subgraph.id && seen_ids.indexOf(subgraph.id) != -1) { continue; }
        content += render_subgraph(subgraph, options, "", seen_ids, todo);
    }

    for (idx in objects) {
        var object = objects[idx];
        if (!is_node(object)) { continue; }
        var node = object;
        console.info("node " + idx + ": " + node);
        if (seen_ids.indexOf(node.id) == -1) {
            content += render_node(node, options);
            todo.push(node);
        }
    }
    while (todo.length > 0) {
        var node = todo.pop(); 
        if (!node.id) { continue; }
        seen_ids.push(node.id);
        for (idx in node) {
            var child = node[idx];
            if (!child.id && !child.is_edge) { continue; }
            var target;
            var edge_data = {};
            console.info("edge " + node.id + "," + child.id + ": " + child);
            if (child.is_edge) {
                target = child.target;
                edge_data = child;
                if (!target) { console.info("child " + child + " is_edge but has no target"); }
            } else {
                target = child;
                if (options && options.infer_labels) {
                    edge_data.label = idx;
                }
                if (options && options.show_node_records) {
                    edge_data.source_port = ":" + idx;
                    edge_data.target_port = ":id";
                }
            }
            console.info("seen_ids: " + seen_ids);
            console.info("todo: " + todo + " target: " + target.id);
            console.info("seen_ids.indexOf(target): " + seen_ids.indexOf(target.id));
            if (seen_ids.indexOf(target.id) == -1) {
                content += render_node(target, options);
                todo.push(target);
            }
            content += render_edge(node, target, edge_data, options);
        }
    }
    return content;
}

// This *sounds* like a pure function, but since the
// callbacks are potentially imperative, it actually
// will dynamically insert first-class edges into the
// graph whereever it finds direct node-to-node links!
// (Override this by passing a non-false value for
// `options.pure_traversal`).
function for_each_reachable(objects, on_node_options, options) {
    var on_node;
    if (!options) { options = on_node_options; }

    if ((typeof on_node_options) == "function") {
        on_node = on_node_options;
    } else {
        on_node = options && options.on_node;
    }

    var on_edge = options && options.on_edge;
    var on_subgraph = options && options.on_subgraph;
    var pure_traversal = options && options.pure_traversal;

    var seen_ids = [];
    var todo = [];

    function push_if(object) {
        if (!object.id || seen_ids.indexOf(object.id) == -1) {
            todo.push(object);
        }
    }

    if (is_node(objects) || is_subgraph(objects)) {
        push_if(objects);
    } else {
        for (idx in objects) {
            var child = objects[idx];
            if (is_node(child)) {
                if (on_node) { on_node(child); }
                push_if(child);
            } else if (is_subgraph(child)) {
                if (on_subgraph) { on_subgraph(child); }
                push_if(child);
            }
        }
    }
    while (todo.length > 0) {
        var object = todo.pop();
        if (!is_node(object) && !is_subgraph(object)) { continue; }

        if (is_subgraph(object)) {
            var subgraph = object;
            if (on_subgraph) { on_subgraph(subgraph); }
            for (idx in subgraph) {
                var sub_object = subgraph[idx];
                if (is_node(sub_object)) {
                    if (seen_ids.indexOf(sub_object.id) == -1) {
                        if (on_node) { on_node(sub_object); }
                        push_if(sub_object);
                    }
                }
            }
        } else if (is_node(object)) {
            var node = object;
            seen_ids.push(node.id);
            for (idx in node) {
                var child = node[idx];
                if (!is_node(child) && !is_edge(child)) { continue; }
                var target;
                var edge_data = {};
                if (is_edge(child)) {
                    target = child.target;
                    edge_data = child;
                    if (!target) { console.info("child " + child + " is_edge but has no target"); }
                } else if (is_node(child)) {
                    target = child;
                    if (!target) { console.info("WTF child " + child + " is false"); }
                    edge_data.target = target;
                    edge_data.is_edge = true;
                    // YIKES! (See note above)
                    if (!pure_traversal) { node[idx] = edge_data; }
                } else {
                    continue;
                }
                console.info("target: " + target);
                if (seen_ids.indexOf(target.id) == -1) {
                    if (on_node) { on_node(target); }
                    push_if(target);
                }
                if (on_edge) { on_edge(edge_data, node, target); }
            }
        }
    }
}

function highlight(object) {
    object.penwidth = "3.0";
    object.color = "red";
    return object;
}

function hide(object) {
    object.style = "invis";
    return object;
}

function unhide(object) {
    if (object.style == "invis") {
        delete object.style;
    }
    return object;
}

function dashed_edge(target) {
    return { is_edge: true, target: target, style: "dashed" };
}

function highlighted_edge(target) {
    return highlight({ is_edge: true,  target: target });
}

function invisible_edge(target) {
    return hide({ is_edge: true,  target: target });
}

function object_record(id, contents) {
    return { id: id, shape: "record",
             label: "<id>" + id + "| " + contents }
}

function edge_from_port(source_port, target) {
    return edge_from_to_ports(source_port, "", target);
}

function edge_to_port(target_port, target) {
    return edge_from_to_ports("", target_port, target);
}

function edge_from_to_ports(source_port, target_port, target) {
    if (source_port.length > 0 && source_port[0] != ':') {
        console.error("source_port "+source_port+" should start with colon");
    }
    if (target_port.length > 0 && target_port[0] != ':') {
        console.error("target_port "+target_port+" should start with colon");
    }
    return { is_edge: true, source_port: source_port, target_port: target_port,
             target: target };
}

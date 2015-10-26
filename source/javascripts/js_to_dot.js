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

function render_objects(objects, options) {
    var content = "";
    var seen_ids = [];
    var todo = [];
    for (idx in objects) {
        var node = objects[idx];
        console.info("node " + idx + ": " + node);
        if (!node.id) { continue; }
        content += render_node(node, options);
        todo.push(node);
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
function for_each_reachable(objects, on_node, on_edge) {
    var seen_ids = [];
    var todo = [];
    if (objects.id) {
        todo.push(objects);
    } else {
        for (idx in objects) {
            var child = objects[idx];
            if (!child.id) { continue; }
            on_node(child);
            todo.push(child);
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
            if (child.is_edge) {
                target = child.target;
                edge_data = child;
            } else {
                target = child;
                edge_data.target = target;
                edge_data.is_edge = true;
                // YIKES! (See note above)
                node[idx] = edge_data;
            }
            if (seen_ids.indexOf(target.id) == -1) {
                if (on_node) { on_node(target); }
                todo.push(target);
            }
            if (on_edge) { on_edge(edge_data, node, target); }
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

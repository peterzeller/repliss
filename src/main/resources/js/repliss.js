var editor = ace.edit("editor");
editor.setTheme("ace/theme/github");
editor.getSession().setMode("ace/mode/repliss");
editor.setShowPrintMargin(false);
editor.setAutoScrollEditorIntoView();
editor.setOptions({
    maxLines: Infinity
});

$(function () {
    var output = $("#output");
    var exampleDropdown = $("#example-dropdown");
    var exampleDropdownSelection = $("#example-dropdown-selection");

    var activeExample = "Userbase";
    var examples = [];

    function loadExamples(data) {
        examples = data;

        exampleDropdown.empty();

        data.forEach(function (ex) {
            var link = $('<a href="#">' + ex.name + '</a>');

            link.click(function () {
                activeExample = ex.name;
                exampleDropdownSelection.text(ex.name);
                output.slideUp();
                loadExamples(examples);
            });

            var li = $('<li>');

            if (activeExample == ex.name) {
                li.addClass('active');
                editor.setValue(ex.code, -1);
            }

            link.appendTo(li);
            li.appendTo(exampleDropdown);

        })

    }

    $.getJSON("/api/examples", {}, loadExamples);


    function setOutput(str, state) {
        output.html(str);
        output.removeClass('bg-danger');
        output.removeClass('bg-info');
        output.removeClass('bg-warning');
        output.removeClass('bg-success');
        if (state == 'error') {
            output.addClass('bg-danger')
        } else if (state == 'warning') {
            output.addClass('bg-warning')
        } else if (state == 'success') {
            output.addClass('bg-success')
        } else {
            output.addClass('bg-info')
        }

    }

    function interpretResponse(data) {
        console.log("interpretResponse", data);
        var listItems = [];
        var valid = false;
        var testFailed = false;
        if (data.verificationResults && data.verificationResults.length > 0) {
            valid = true;
            data.verificationResults.forEach(function (res) {
                listItems.push($("<li>" + res.resState + ": " + res.proc + "</li>"));
                if (res.resState != 'valid') {
                    valid = false;
                }
            })
        } else if (data.errors && data.errors.length > 0) {
            data.errors.forEach(function (err) {
                listItems.push($("<li>Error in line " + err.line + ": " + err.message + "</li>"))
            })
        }


        var list = $("<ul>");
        listItems.forEach(function (li) {
            list.append(li);
        });

        var output = $("<div>");
        output.append(list);

        if (data.counterexample) {
            var ce = data.counterexample;
            if (ce === 'none') {
                output.append($("<p>").text("No counter example found."));
            } else {
                output.append($("<p>").text("Found a counter example, invariant in line " + ce.invline + " failed."));
                var svg = $(ce.svg);
                svg.find("polygon").first().remove();
                output.append(svg);
                valid = false;
            }
        }

        var state = valid ? 'success' : 'error';
        setOutput(output, state);
    }

    var btnVerify = $("#btn-verify");
    btnVerify.click(function () {
        var contents = editor.getValue();
        btnVerify.addClass('running');
        output.slideUp();


        var xhr = new XMLHttpRequest();
        xhr.open('POST', "/api/check", true);
        xhr.send(JSON.stringify({
            code: contents
        }));



        var timer;
        var lastText = "";
        timer = window.setInterval(function () {
            if (xhr.readyState === XMLHttpRequest.DONE) {
                window.clearTimeout(timer);
                btnVerify.removeClass('running');
            }


            var parser = new DOMParser();
            var text = xhr.responseText;
            if (text.length <= lastText.length) {
                return;
            }
            lastText = text;
            if (text.indexOf("</results>") < 0) {
                text += "</results>";
            }
            console.log(text);
            var xml = parser.parseFromString(text, "text/xml");

            var data = {
                verificationResults: [],
                errors: []
            };

            var results = xml.childNodes.item(0);
            for (var i = 0; i < results.childNodes.length; i++) {
                var child = results.childNodes.item(i);
                if (child.nodeName === 'verificationResult') {
                    data.verificationResults.push({
                        resState: child.getAttribute("resState"),
                        proc: child.getAttribute("proc")
                    });

                } else if (child.nodeName === 'nocounterexample') {
                    data.counterexample = 'none';
                } else if (child.nodeName === 'counterexample') {
                    data.counterexample = {
                        invline: child.getAttribute("invline"),
                        svg: child.textContent
                    }
                } else if (child.nodeName === 'error') {
                    data.errors.push({
                        line: child.getAttribute("line"),
                        column: child.getAttribute("column"),
                        endline: child.getAttribute("endline"),
                        endcolumn: child.getAttribute("endcolumn"),
                        message: child.getAttribute("message")
                    })
                }
            }
            interpretResponse(data);
            output.slideDown();

        }, 100);

        // $.ajax({
        //     method: "POST",
        //     url: "/api/check",
        //     data: JSON.stringify({
        //         code: contents
        //     }),
        //     contentType: 'application/json; charset=UTF-8',
        //     dataType: "json",
        //     timeout: 0,
        //     success: function (data) {
        //         interpretResponse(data);
        //     },
        //     error: function(req, textStatus, errorThrown) {
        //         setOutput("Failed to process request! (" + textStatus + ", " + errorThrown + ")", 'error')
        //     }
        // }).always(function () {
        //     btnVerify.removeClass('running');
        //     output.slideDown();
        // })

    })

});



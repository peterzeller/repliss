var editor = ace.edit("editor");
editor.setTheme("ace/theme/github");
editor.getSession().setMode("ace/mode/repliss");
editor.setShowPrintMargin(false);
editor.setAutoScrollEditorIntoView();
editor.setOptions({
    maxLines: Infinity
});


// Cookie functions from http://stackoverflow.com/questions/1458724/how-do-i-set-unset-cookie-with-jquery
function createCookie(name, value, days) {
    var expires;

    if (days) {
        var date = new Date();
        date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
        expires = "; expires=" + date.toGMTString();
    } else {
        expires = "";
    }
    document.cookie = encodeURIComponent(name) + "=" + encodeURIComponent(value) + expires + "; path=/";
}

function readCookie(name) {
    var nameEQ = encodeURIComponent(name) + "=";
    var ca = document.cookie.split(';');
    for (var i = 0; i < ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0) === ' ') c = c.substring(1, c.length);
        if (c.indexOf(nameEQ) === 0) return decodeURIComponent(c.substring(nameEQ.length, c.length));
    }
    return null;
}

function eraseCookie(name) {
    createCookie(name, "", -1);
}

function isInt(value) {
    var x;
    if (isNaN(value)) {
        return false;
    }
    x = parseFloat(value);
    return (x | 0) === x;
}

function setEditorFontsize(size) {
    editor.setOptions({
        fontSize: size
    });
}

$(function () {
    // font size:


    function changeFontsize(diff) {
        var size = editor.getFontSize();
        size += diff;
        createCookie('repliss-font-size', size);
        setEditorFontsize(size);
    }

    var storedSize = readCookie('repliss-font-size');
    if (storedSize) {
        setEditorFontsize(parseInt(storedSize));
    }

    $("#decreaseEditorFont").click(function(e) { changeFontsize(-1) });
    $("#increaseEditorFont").click(function(e) { changeFontsize(1) });




    var output = $("#output");
    var exampleDropdown = $("#example-dropdown");
    var exampleDropdownSelection = $("#example-dropdown-selection");


    var examples = [];

    function loadExamples(data) {
        examples = data;
        var activeExample = examples[0].name;
        examples.forEach(function (ex) {
            if (window.location.hash.replace("#", "") === ex.name) {
                activeExample = ex.name;
            }
        });

        exampleDropdownSelection.text(activeExample);


        exampleDropdown.empty();

        data.forEach(function (ex) {
            var link = $('<a href="#">' + ex.name + '</a>');

            link.click(function (event) {
                window.location.hash = ex.name;
                output.slideUp();
                loadExamples(examples);
                event.preventDefault();
            });

            var li = $('<li>');

            if (activeExample === ex.name) {
                li.addClass('active');
                editor.setValue(ex.code, -1);
            }

            link.appendTo(li);
            li.appendTo(exampleDropdown);

        })

    }

    $.getJSON("./api/examples", {}, loadExamples);


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
                var info = ce.info;
                if (info) {
                    var infoText = $("<p>");
                    infoText.text("Additional information:;" + info)
                    info = infoText.html();
                    info = info.replace(";", "<br />")
                    infoText.html(info);
                    output.append(infoText);
                }

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
        xhr.open('POST', "./api/check", true);
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
                        info: child.getAttribute("info"),
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

    })




});



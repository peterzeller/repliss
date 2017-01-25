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
        if (data.verificationResults) {
            valid = true;
            data.verificationResults.forEach(function (res) {
                listItems.push($("<li>" + res.resState + ": " + res.proc + "</li>"));
                if (res.resState != 'valid') {
                    valid = false;
                }
            })
        } else if (data.errors) {
            data.errors.forEach(function (err) {
                listItems.push($("<li>Error in line " + err.line + ": " + err.message + "</li>"))
            })
        }

        var list = $("<ul>");
        listItems.forEach(function (li) {
            list.append(li);
        });
        var state = valid ? 'success' : 'error';
        setOutput(list, state);
    }

    var btnVerify = $("#btn-verify");
    btnVerify.click(function () {
        var contents = editor.getValue();
        btnVerify.addClass('running');
        output.slideUp();

        $.post("/api/check", JSON.stringify({
            code: contents
        }), function (data) {
            interpretResponse(data);
        }).fail(function () {
            setOutput("Failed to process request!", 'error')

        }).always(function () {
            btnVerify.removeClass('running');
            output.slideDown();
        })

    })

});



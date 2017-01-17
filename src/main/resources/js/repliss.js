var editor = ace.edit("editor");
editor.setTheme("ace/theme/github");
editor.getSession().setMode("ace/mode/repliss");
editor.setShowPrintMargin(false);
editor.setOptions({
    maxLines: Infinity
});

$(function () {
    var output = $("#output");

    function setOutput(str, state) {
        output.html(str);
        output.removeClass('bg-danger')
        output.removeClass('bg-info')
        output.removeClass('bg-warning')
        output.removeClass('bg-success')
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
                listItems.push($("<li>" + res.resState + ": " + res.proc + "</li>"))
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



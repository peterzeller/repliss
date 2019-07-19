if (process.env.NODE_ENV === "production") {
    const opt = require("./repliss-js-opt.js");
    opt.main();
    module.exports = opt;
} else {
    var exports = window;
    exports.require = require("./repliss-js-fastopt-entrypoint.js").require;
    window.global = window;

    const fastOpt = require("./repliss-js-fastopt.js");
    fastOpt.main()
    module.exports = fastOpt;

    if (module.hot) {
        module.hot.accept();
    }
}

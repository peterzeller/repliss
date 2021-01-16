package crdtver.web

import scalatags.Text.TypedTag
import scalatags.Text.all.{link, _}

class MainPage {


  def mainTemplate(): TypedTag[Predef.String] = {
    html(
      head(
        meta(charset := "utf-8"),
        meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        tag("title")("Repliss"),
        link(href := "/webjars/bootstrap/3.1.1/css/bootstrap.css", rel := "stylesheet"),
        link(href := "/css/style.css", rel := "stylesheet"),
        link(href := "//fonts.googleapis.com/css?family=Niconne", rel := "stylesheet")
        // <link rel="stylesheet" type="text/css" href="//fonts.googleapis.com/css?family=Niconne" />
        //        <link href="css/bootstrap.min.css" rel="stylesheet">
      ),
      body(
        div(`class` := "container-fluid")(
          h1()("Repliss"),
          "Hello World",
          div(id := "editor")
        ),
        script(src := "/webjars/ace/01.08.2014/src-noconflict/ace.js", `type`:= "text/javascript"),
        script(src := "/js/repliss.js", `type`:= "text/javascript")
      )
    )

  }

}

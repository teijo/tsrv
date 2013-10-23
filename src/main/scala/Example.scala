package com.example

import unfiltered.request._
import unfiltered.response._
import org.json4s._
import org.json4s.native.Serialization.write

import unfiltered.directives._, Directives._

case class CreateResponse(id: Integer)
case class Tournament(id: Integer, teams: List[String])

class App extends unfiltered.filter.Plan {
  implicit val formats = native.Serialization.formats(NoTypeHints)

  def intent = Directive.Intent {
    case GET(Path("/")) =>
      success(Ok ~> ResponseString("Hello"))

    case GET(Path("/record")) =>
      success(Ok ~> ResponseString(write(Tournament(id = 123, teams = List("A", "B")))))

    case POST(Path("/record")) =>
      success(Ok ~> ResponseString(write(CreateResponse(123))))

    case PUT(Path("/record")) =>
      success(Ok ~> ResponseString(write(CreateResponse(123))))
  }
}

object Server {
  def main(args: Array[String]) {
    unfiltered.jetty.Http.local(8080).filter(new App).run({ svr =>
      unfiltered.util.Browser.open(svr.url)
    }, { svr =>
    })
  }
}

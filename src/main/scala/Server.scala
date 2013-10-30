package tsrv

import unfiltered.request._
import unfiltered.response._
import org.json4s._
import org.json4s.native.Serialization.write
import org.json4s.native.Serialization.read

import unfiltered.directives._, Directives._


case class Team(id: Integer, name: String)
case class GroupMatchTeam(team: Integer, score: Integer)
case class GroupMatch(round: Integer, a: GroupMatchTeam, b: GroupMatchTeam)
case class GroupRequest(teams: List[Team], matches: List[GroupMatch])

case class BracketRequest(teams: List[List[Team]], results: List[List[List[List[Integer]]]])

case class CreateResponse(id: Integer)
case class Tournament(id: Integer, teams: List[String])

class App extends unfiltered.filter.Plan {
  implicit val formats = native.Serialization.formats(NoTypeHints)

  def intent = Directive.Intent {
    case OPTIONS(Path("/record")) =>
      success(Ok ~> ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseHeader("Access-Control-Allow-Headers", Set("Content-Type", "Authorization", "X-Requested-With")) ~>
        ResponseHeader("Access-Control-Allow-Methods", Set("GET", "POST", "PUT", "OPTIONS")))

    case GET(Path("/")) =>
      success(Ok ~> ResponseString("Hello"))

    case GET(Path("/record")) =>
      success(Ok ~> ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseString(write(Tournament(id = 123, teams = List("A", "B")))))

    case req @ POST(Path("/record")) =>
      val bracketRecord = read[BracketRequest](Body.string(req))
      println(bracketRecord)
      success(Ok ~> ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseString(write(bracketRecord)))

    case req @ PUT(Path("/record")) =>
      val groupRecord = read[GroupRequest](Body.string(req))
      success(Ok ~> ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseString(write(groupRecord)))
  }
}

object Server {
  def main(args: Array[String]) {
    unfiltered.jetty.Http.local(8080).context("/js") {
      _.resources(new java.net.URL(getClass().getResource("/www/js/t.js"), "."))
    }.filter(new App).run({ svr =>
      unfiltered.util.Browser.open(svr.url)
    }, { svr =>
    })
  }
}

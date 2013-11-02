package tsrv

import unfiltered.request._
import unfiltered.response._
import org.json4s._
import org.json4s.native.Serialization.write
import org.json4s.native.Serialization.read

import unfiltered.directives._, Directives._
import org.json4s.ParserUtil.ParseException
import scalaj.http.{HttpOptions, Http}

case class Team(id: Integer, name: String)
case class GroupMatchTeam(team: Integer, score: Integer)
case class GroupMatch(round: Integer, a: GroupMatchTeam, b: GroupMatchTeam)
case class Group(teams: List[Team], matches: List[GroupMatch])
case class Bracket(teams: List[List[Team]], results: List[List[List[List[Integer]]]])

class App extends unfiltered.filter.Plan {
  implicit val formats = native.Serialization.formats(NoTypeHints)

  def intent = Directive.Intent {
    case OPTIONS(Path(Seg(_ :: Nil))) =>
      success(Ok ~> ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseHeader("Access-Control-Allow-Headers", Set("Content-Type", "Authorization", "X-Requested-With")) ~>
        ResponseHeader("Access-Control-Allow-Methods", Set("GET", "POST", "PUT", "OPTIONS")))

    case req @ GET(Path("/")) =>
      out(() => "Hello")

    case req @ Path("/bracket") => req match {
      case POST(_) =>
        out(() => {
          val groupJSON = write(Bracket(teams = List(), results = List()))
          val url = "http://127.0.0.1:8098/riak/brackets/test"
          val response = Http.postData(url, groupJSON).header("content-type", "application/json")
          println(s"Create bracket: ${response.responseCode}")
          groupJSON
        })
      case PUT(_) =>
        out(() => write(read[Bracket](Body.string(req))))
    }

    case req @ Path("/group") => req match {
      case POST(_) =>
        out(() => {
          val groupJSON = write(Group(teams = List(), matches = List()))
          val url = "http://127.0.0.1:8098/riak/groups/test"
          val response = Http.postData(url, groupJSON).header("content-type", "application/json")
          println(s"Create group: ${response.responseCode}")
          groupJSON
        })
      case PUT(_) =>
        out(() => write(read[Group](Body.string(req))))
    }
  }

  def out(createResponse: () => String): Directive[Any, Nothing, AnyRef with ResponseFunction[Any]] = {
    try {
      val response: String = createResponse()
      success(Ok ~>
        ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseString(response))
    } catch {
      case _: ParseException => success(BadRequest ~>
        ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseString("""{"error":"Invalid input JSON"}"""))
    }
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

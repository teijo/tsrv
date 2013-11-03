package tsrv

import tsrv.TournamentType.TournamentType
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

object TournamentType extends Enumeration {
  type TournamentType = Value
  val Bracket = Value("bracket")
  val Group = Value("group")
}

class App extends unfiltered.filter.Plan {
  val server = "http://127.0.0.1:8098/riak"
  implicit val formats = native.Serialization.formats(NoTypeHints)

  def intent = Directive.Intent {
    case OPTIONS(Path(Seg(_ :: Nil))) | OPTIONS(Path(Seg(_ :: _ :: Nil))) =>
      success(Ok ~> ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseHeader("Access-Control-Allow-Headers", Set("Content-Type", "Authorization", "X-Requested-With")) ~>
        ResponseHeader("Access-Control-Allow-Methods", Set("GET", "POST", "PUT", "OPTIONS")))

    case req @ GET(Path("/")) =>
      out(() => "Hello")

    case req @ Path("/bracket") => req match {
      case POST(_) =>
        out(() => {
          val jsonString = write(Bracket(teams = List(), results = List()))
          val status = dbCreate(TournamentType.Bracket, jsonString)
          jsonString
        })
    }

    case req @ Path("/group") => req match {
      case POST(_) =>
        out(() => {
          val jsonString = write(Group(teams = List(), matches = List()))
          val status = dbCreate(TournamentType.Group, jsonString)
          jsonString
        })
    }

    case req @ Path(Seg("group" :: id :: Nil)) => req match {
      case PUT(_) =>
        val jsonString = write(read[Group](Body.string(req)))
        dbUpdate(TournamentType.Group, id, jsonString)
        out(() => jsonString)
      case req @ GET(_) =>
        out(() => dbRead(TournamentType.Group, id))
    }

    case req @ Path(Seg("bracket" :: id :: Nil)) => req match {
      case PUT(_) =>
        val jsonString = write(read[Bracket](Body.string(req)))
        dbUpdate(TournamentType.Bracket, id, jsonString)
        out(() => jsonString)
      case req @ GET(_) =>
        out(() => dbRead(TournamentType.Bracket, id))
    }

  }

  def dbCreate(tType: TournamentType, jsonData: String): Integer = {
    val url = s"${server}/${tType}"
    val (status, headers, body) = Http.postData(url, jsonData).header("content-type", "application/json").asHeadersAndParse(Http.readString)
    status
  }

  def dbUpdate(tType: TournamentType, id: String, jsonData: String): Integer = {
    val url = s"${server}/${tType}/${id}"
    val (status, headers, body) = Http.postData(url, jsonData).header("content-type", "application/json").asHeadersAndParse(Http.readString)
    status
  }

  def dbRead(tType: TournamentType, id: String): String = {
    val url = s"${server}/${tType}/${id}"
    Http.get(url).asString
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

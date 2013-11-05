package tsrv

import tsrv.TournamentType.TournamentType
import unfiltered.request._
import unfiltered.response._
import org.json4s._
import org.json4s.native.Serialization.write
import org.json4s.native.Serialization.read

import unfiltered.directives._, Directives._
import org.json4s.ParserUtil.ParseException
import scalaj.http.{HttpException, HttpOptions, Http}

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
  val serverRoot = "http://127.0.0.1:8098"
  val serverData = s"${serverRoot}/riak"
  implicit val formats = native.Serialization.formats(NoTypeHints)

  def intent = Directive.Intent {
    case OPTIONS(Path(Seg(_ :: Nil))) | OPTIONS(Path(Seg(_ :: _ :: Nil))) =>
      success(Ok ~> ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseHeader("Access-Control-Allow-Headers", Set("Content-Type", "Authorization", "X-Requested-With")) ~>
        ResponseHeader("Access-Control-Allow-Methods", Set("GET", "POST", "PUT", "OPTIONS")))

    case req @ GET(Path("/")) =>
      out(() => ("Hello", Ok))

    case req @ Path("/bracket") =>
      implicit val tType = TournamentType.Bracket
      req match {
        case GET(_) =>
          out(() => dbList())
        case POST(_) =>
          out(() => {
            val jsonString = write(Bracket(teams = List(), results = List()))
            dbCreate(jsonString)
          })
      }

    case req @ Path("/group") =>
      implicit val tType = TournamentType.Group
       req match {
         case GET(_) =>
           out(() => dbList())
         case POST(_) =>
           out(() => {
             val jsonString = write(Group(teams = List(), matches = List()))
             dbCreate(jsonString)
           })
       }

    case req @ Path(Seg("group" :: id :: Nil)) =>
      implicit val tType = TournamentType.Group
      req match {
        case PUT(_) =>
          val jsonString = write(read[Group](Body.string(req)))
          out(() => dbUpdate(id, jsonString))
        case GET(_) =>
          out(() => dbRead(id))
        case DELETE(_) =>
          out(() => dbDelete(id))
      }

    case req @ Path(Seg("bracket" :: id :: Nil)) =>
      implicit val tType: TournamentType = TournamentType.Bracket
      req match {
        case PUT(_) =>
          val jsonString = write(read[Bracket](Body.string(req)))
          out(() => dbUpdate(id, jsonString))
        case GET(_) =>
          out(() => dbRead(id))
        case DELETE(_) =>
          out(() => dbDelete(id))
      }

  }

  def dbList()(implicit tType: TournamentType): (String, Status) = {
    val url = s"${serverRoot}/buckets/${tType}/keys?keys=true"
    val (status, headers, body) = Http.get(url).asHeadersAndParse(Http.readString)
    (body, Ok)
  }

  def dbDelete(id: String)(implicit tType: TournamentType): (String, Status) = {
    val url = s"${serverData}/${tType}/${id}"
    try {
      val (status, headers, body) = Http(url).option(HttpOptions.method("DELETE")).asHeadersAndParse(Http.readString)
      ( """{"delete":"ok"}""", Ok)
    } catch {
      case e: HttpException =>
        ( """{"error":"Not found"}""", NotFound)
    }
  }

  def dbCreate(jsonData: String)(implicit tType: TournamentType): (String, Status) = {
    val url = s"${serverData}/${tType}"
    val (status, headers, body) = Http.postData(url, jsonData).header("content-type", "application/json").asHeadersAndParse(Http.readString)
    (jsonData, Ok)
  }

  def dbUpdate(id: String, jsonData: String)(implicit tType: TournamentType): (String, Status) = {
    val url = s"${serverData}/${tType}/${id}"
    val (status, headers, body) = Http.postData(url, jsonData).header("content-type", "application/json").asHeadersAndParse(Http.readString)
    (jsonData, Ok)
  }

  def dbRead(id: String)(implicit tType: TournamentType): (String, Status) = {
    val url = s"${serverData}/${tType}/${id}"
    try {
      val (status, headers, body) = Http.get(url).asHeadersAndParse(Http.readString)
      (body, Ok)
    } catch {
      case e: HttpException =>
        ("""{"error":"Not found"}""", NotFound)
    }
  }

  def out(createResponse: () => (String, Status)): Directive[Any, Nothing, AnyRef with ResponseFunction[Any]] = {
    try {
      val (response: String, status: Status) = createResponse()
      success(status ~>
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

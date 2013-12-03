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

case class Team(id: Integer, name: String, format: String, data: Map[String,String])
case class GroupMatchTeam(team: Integer, score: Integer)
case class GroupMatch(round: Integer, a: GroupMatchTeam, b: GroupMatchTeam)
case class Group(teams: List[Team], matches: List[GroupMatch])
case class Bracket(teams: List[List[Team]], results: List[List[List[List[Integer]]]])
case class CreateGroup(id: String, data: Group)
case class CreateBracket(id: String, data: Bracket)
case class User(username: String, password: String, email: String)
case class CreateUser(id: String, data: User)
case class LoginRequest(username: String)
case class LoginResponse(token: String)

object TournamentType extends Enumeration {
  type TournamentType = Value
  val Bracket = Value("bracket")
  val Group = Value("group")
  val User = Value("user")
}

class App extends unfiltered.filter.Plan {
  val serverRoot = "http://127.0.0.1:8098"
  val serverData = s"${serverRoot}/riak"
  implicit val formats = native.Serialization.formats(NoTypeHints)

  def intent = Directive.Intent {
    case OPTIONS(Path(Seg(_ :: Nil))) | OPTIONS(Path(Seg(_ :: _ :: Nil))) =>
      success(Ok ~> ResponseHeader("Access-Control-Allow-Origin", Set("*")) ~>
        ResponseHeader("Access-Control-Allow-Headers", Set("Content-Type", "Authorization", "X-Requested-With")) ~>
        ResponseHeader("Access-Control-Allow-Methods", Set("GET", "POST", "PUT", "DELETE", "OPTIONS")))

    case req @ GET(Path("/")) =>
      out(() => ("Hello", Ok))

    case req @ Path("/bracket") =>
      implicit val tType = TournamentType.Bracket
      req match {
        case GET(_) =>
          out(() => dbList())
        case POST(_) =>
          out(() => {
            val bracket: Bracket = Bracket(teams = List(List(
              Team(id = 1, name = "Team 1", format = "", data = Map()),
              Team(id = 2, name = "Team 2", format = "", data = Map())
            )), results = List())
            val jsonString = write(bracket)
            val (id, status) = dbCreate(jsonString)
            (write(CreateBracket(id = id, data = bracket)), status)
          })
      }

    case req @ Path("/group") =>
      implicit val tType = TournamentType.Group
       req match {
         case GET(_) =>
           out(() => dbList())
         case POST(_) =>
           out(() => {
             val group: Group = Group(teams = List(), matches = List())
             val jsonString = write(group)
             val (id, status) = dbCreate(jsonString)
             (write(CreateGroup(id = id, data = group)), status)
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

    case req @ Path("/user") =>
      implicit val tType = TournamentType.User
      req match {
        case POST(_) =>
          out(() => {
            val user: User = read[User](Body.string(req))
            val jsonString = write(user)
            val (id, status) = dbUpdate(user.username, jsonString)
            (write(CreateUser(id = id, data = user)), status)
          })
      }

    case req @ Path("/login") =>
      implicit val tType = TournamentType.User
      req match {
        case POST(_) =>
          out(() => {
            val login: LoginRequest = read[LoginRequest](Body.string(req))
            val (_, status): (String, Status) = dbRead(login.username)
            if (status == 200)
              (write(LoginResponse(token = "t0ken")), Ok)
            else
              (write(LoginResponse(token = "")), status)
          })
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
    val id: String = headers("Location").head.split("/").last
    (id, Ok)
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

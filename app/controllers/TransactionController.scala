package controllers

import play.api.mvc._
import models._
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


object TransactionController extends Controller {

  val tryJsonParser: BodyParser[Try[JsValue]] = parse.tolerantText.map(text => Try(Json.parse(text)))
  implicit val transactionRequestFormat = Json.format[TransactionRequest]
  implicit val sumFormat = Json.format[Sum]


  def getTransaction(transactionID: Long) = Action {
    Ok(Json.toJson(TransactionModel.get(transactionID).map(x => TransactionRequest(x.amount, x.transactionType, x.parentID))))
  }

  def insertTransaction(transactionID: Long) = Action(tryJsonParser) { request =>
    request.body match {
      case Success(js) =>
        js.validate[TransactionRequest].map {
          case TransactionRequest(amount, transactionType, parentId) =>
            val newTrans = Transaction(transactionID, amount, transactionType, parentId)
            TransactionModel.insert(newTrans, parentId)
            Ok(Json.toJson(Map("status" -> "ok")))
        }.recoverTotal {
          e => BadRequest(Json.obj("status" -> "ko", "message" -> JsError.toJson(e)))
        }
      case Failure(ex) => BadRequest(Json.obj("status" -> "KO", "message" -> "Expecting text/json or application/json body"))
    }
  }

  def getTransactionByType(transactionType: String) = Action {
    val listIDs = TransactionModel.getByType(transactionType).map(_.id)

    Ok(Json.toJson(listIDs))
  }

  def getSumTransaction(transactionID: Long) = Action {
    val transSum = Sum(TransactionModel.sum(transactionID))
    Ok(Json.toJson(transSum))
  }
}

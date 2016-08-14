package models

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TransactionModel {

  def insert(transaction: Transaction, parentId: Option[Long] = None) = {
    get(transaction.id).getOrElse {
      val tempTrans = parentId.map(parent => get(parent).map(_.addChild(transaction))
        .getOrElse(transaction.copy(parentID = None)))
        .getOrElse(transaction)
      TransactionSet.insertTransaction(tempTrans.id, tempTrans)
      TransactionSet.insertTypes(tempTrans.transactionType, tempTrans)
      tempTrans
    }
  }

  def get(id:Long):Option[Transaction] = {
    TransactionSet.getTransaction(id)
  }

  def sum(id:Long):Double = {
    get(id).map(_.getTotalAmount).getOrElse(0)
  }

  def getByType(transactionType: String):List[Transaction] = {
    TransactionSet.getTypes(transactionType).toList
  }
}

case class Sum(sum:Double)
case class TransactionRequest(amount: Double, `type`: String, parent_id: Option[Long])

case class Transaction(id:Long, amount: Double, transactionType: String, parentID: Option[Long]) {

  private val children = ListBuffer.empty[Transaction]

  def addChild(transaction: Transaction) = {
    children += transaction
    transaction
  }

  /**
    * get children of a transaction
    * @return
    */
  def getChildren = {
    children
  }

  /**
    * check if transaction have children or not
    * @return
    */
  def hasChildren:Boolean = {
    if(children.nonEmpty) true else false
  }

  def getTotalAmount:Double = {
    amount + getChildren.toList.map(_.getTotalAmount).sum
  }
}

/**
  * Keep all the transaction
  */
object TransactionSet{
  private val map = mutable.HashMap.empty[Long,Transaction]
  private val transTypes = mutable.Map.empty[String, mutable.ListBuffer[Transaction]]

  def insertTransaction(id:Long, transaction: Transaction) = {
    if(!map.contains(id)) map += (id -> transaction)
  }

  def getTransaction(id:Long): Option[Transaction] = {
    if(map.contains(id)) {
      Some(map(id))
    } else {
      None
    }
  }

  def insertTypes(transactionType: String, transaction: Transaction) = {
    if (transTypes.contains(transactionType)) {
      transTypes += (transactionType -> (transTypes(transactionType) += transaction))
    } else {
      transTypes += (transactionType -> ListBuffer(transaction))
    }
  }

  def getTypes(transactionType: String) = {
    if (transTypes.contains(transactionType)) transTypes(transactionType) else ListBuffer.empty[Transaction]
  }
}


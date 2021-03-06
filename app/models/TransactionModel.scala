package models

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TransactionModel {

  /**
    * Insert transaction to data set
    * @param transaction
    * @param parentId
    * @return
    */
  def insert(transaction: Transaction, parentId: Option[Long] = None) = {
    get(transaction.id).getOrElse {
      val tempTrans = parentId.map(parent => get(parent).map(_.addChild(transaction))
        .getOrElse(transaction.copy(parentID = None)))
        .getOrElse(transaction)
      TransactionSet.insertTransaction(tempTrans.id, tempTrans)
      TransactionSet.insertType(tempTrans.transactionType, tempTrans)
      tempTrans
    }
  }

  /**
    * get a transaction by id
    * @param id
    * @return
    */
  def get(id:Long):Option[Transaction] = {
    TransactionSet.getTransaction(id)
  }

  /**
    * Return sum of all transactions that are transitively linked by their parent_id
    * @param id
    * @return
    */
  def sum(id:Long):Double = {
    get(id).map(_.getTotalAmount).getOrElse(0)
  }

  /**
    * Get list of ids with the same transaction type
    * @param transactionType
    * @return
    */
  def getByType(transactionType: String):List[Transaction] = {
    TransactionSet.getWithType(transactionType).toList
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

  /**
    * Insert transaction to dataset
    * only insert that data that doesn't exist
    * @param id
    * @param transaction
    * @return
    */
  def insertTransaction(id:Long, transaction: Transaction) = {
    if(!map.contains(id)) map += (id -> transaction)
  }

  /**
    * get transaction from dataset
    * @param id
    * @return
    */
  def getTransaction(id:Long): Option[Transaction] = {
    if(map.contains(id)) {
      Some(map(id))
    } else {
      None
    }
  }

  /**
    * insert transaction with with same types
    * @param transactionType
    * @param transaction
    * @return
    */
  def insertType(transactionType: String, transaction: Transaction) = {
    if (transTypes.contains(transactionType)) {
      transTypes += (transactionType -> (transTypes(transactionType) += transaction))
    } else {
      transTypes += (transactionType -> ListBuffer(transaction))
    }
  }

  /**
    * get transaction with same type
    * @param transactionType
    * @return
    */
  def getWithType(transactionType: String) = {
    if (transTypes.contains(transactionType)) transTypes(transactionType) else ListBuffer.empty[Transaction]
  }
}


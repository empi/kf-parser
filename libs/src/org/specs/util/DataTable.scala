package org.specs.util
import org.specs.Products._
import scala._
import org.specs.specification._
import scala.xml.NodeSeq
/**
 * The Datatables trait provides implicit methods to start table headers 
 * and DataRows.<p>
 */
trait DataTables {
  /**
   * @return a table header which first column is the string <code>a</code> 
   */
  implicit def toTableHeader(a: String) = new TableHeader(List(a))

  /**
   * @return a table row whose type will be <code>T</code> for each element and
   * which starts with <code>a</code> as the first element 
   */
  implicit def toDataRow[T](a: T) = DataRow1(a)
}

/**
 * The TableHeader case class models the header of a data table which should be a list of strings.<p>
 * A header can be created using the | operator a separator between strings<pre>
 * "a" | "b" | "c = a + b"|
 * </pre>
 * A header can be closed using the | method which will return the TableHeader object<p>
 * A header can be followed by data rows which only requirement is to have a <code>def header_=(t: TableHeader)</code> function
 */
case class TableHeader(val titles: List[String], var isOk: Boolean) {
  /** Mutator to indicate a failure in the rest of the table. */
  def setFailed() = isOk = false

  /** Default constructor. */
  def this(titles: List[String]) = this(titles, true)
  /**
   * Adds a new column to the header
   * @returns the extended header
   */
  def |(s: String) = TableHeader(titles ::: List(s), isOk)

  /**
  * Used to close the header
  * @returns the header
  */
  def | = this

  /**
   * Accepts any object on which a header can be set. Sets the header on that object and returns it
   * @returns the header-accepting object (usually a DataRow object)
   */
  def |[T <: Any {def header_=(t: TableHeader)}](d: T) = {d.header_=(this); d}

  /**
   * Accepts any object on which a header can be set and which is executable. 
   * Sets the header on the object, marks it as "should be executed" and returns it.
   * @returns the header-accepting object (usually a DataRow object), ready to execute
   */
  def |>[T <: Any {def header_=(t: TableHeader); def shouldExecute_=(b: Boolean)}](d: T) = {d.header_=(this); d.shouldExecute_=(true); d}

  /**
   * @returns the header as string: |"a" | "b" | "c = a + b"|
   */
   override def toString = titles.mkString("|", "|", "|")
  /**
   * @returns the header as html
   */
   def toHtml = {
     <tr>{
       titles.map((t:Any) => <th>{t.toString}</th>)}{
       if (!isOk) <th><img src="images/icon_failure_sml.gif"/></th> else NodeSeq.Empty
     }</tr>
   }
}

/**
 * Abstract representation of a row without the column types.
 */
trait AbstractDataRow extends DefaultResults { 
  var header: TableHeader = new TableHeader(Nil, true)
  var shouldExecute = false;
  def valuesList: List[Any]
}
abstract class DataRow[+T0, +T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, 
                       +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19](val values: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)) extends AbstractDataRow {
  def | = this
  def |[S0 >: T0, S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9, 
        S10 >: T10, S11 >: T11, S12 >: T12, S13 >: T13, S14 >: T14, 
        S15 >: T15, S16 >: T16, S17 >: T17, S18 >: T18, S19 >: T19](row: DataRow[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19]): DataTable[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19] = DataTable(header, List(this, {row.header_=(header); row}), shouldExecute)
  def |>[S0 >: T0, S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9, 
        S10 >: T10, S11 >: T11, S12 >: T12, S13 >: T13, S14 >: T14, 
        S15 >: T15, S16 >: T16, S17 >: T17, S18 >: T18, S19 >: T19](row: DataRow[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19]): DataTable[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19] = DataTable(header, List(this, {row.header_=(header); row}), true)
  def valuesList = {
    var l: List[Any] = Nil
    for (i <- new Range(0, values.productArity, 1);
         e <- values.productElement(i) if (e != None))
           l = l:::List(e)
    l
  }
  override def toString = valuesList.mkString("|", "|", "|")
  def result = statusAsText + toString + (if (isOk) "" else " ") + issues.map(_.getMessage).mkString(",") 
  def toHtml = {
    <tr class={status}>{
      valuesList.map((v:Any) => <td>{v.toString}</td>)}{
      if (header.isOk) NodeSeq.Empty else <td>{issues.map(_.getMessage)}</td>
    }</tr>
  }
}
trait ExecutableDataTable extends HasResults {
  def execute: this.type
  def results: String
  def rows: List[AbstractDataRow]
  def toHtml: NodeSeq
  def failures: Seq[FailureException] = rows.flatMap(_.failures)
  def errors: Seq[Throwable] = rows.flatMap(_.errors)
  def skipped: Seq[SkippedException] = rows.flatMap(_.skipped)
  def reset() = { rows.foreach { r => r.reset() }}
}

/**
 * A DataTable contains: a header describing the columns, datarows containing values, a function to execute on each row
 * In the following example of a DataTable:<code>
 * val datatable = "a" | "b" | "c = a + b" |>
 *                  1  !  2  !     3       | 
 *                  2  !  2  !     4       | 
 *                  3  !  2  !     5       | {
 *                (a: Int, b: Int, c: Int) => c must_== calc.add(a, b)
 *                 } </code>
 * The header defines 3 columns, there are 3 rows of type (Int, Int, Int) and a function taking its values in each row.
 * The '>' sign added to the header means that the function will be executed on each row when the table is defined. The result
 * of the execution will be available via a <code>results</code> function returning a string.<p>
 * A DataTable has the following constraints:<ul>
 * <li/>It cannot have more than 20 columns
 * <li/>The rows must be of identical types
 * <li/>The function must have the same parameter types as the types of the rows (However the function can have less parameters than the number of row cells)
 * </ul>
 */
case class DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](header: TableHeader, rows: List[DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]], var shouldExecute: Boolean) extends ExecutableDataTable { outer =>
  private var tableFailureFunction: DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] => Unit = { t => throw DataTableFailureException(t) }
  
  type AbstractDataRow = DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
  /**
   * This function can be overriden to provide another behaviour upon table failure
   */  
  def failureFunction(table: DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) : Unit = {
    header.setFailed()
    tableFailureFunction(table) 
  }
  /**
   * This sets a failing function to use when the table fails
   */  
  def whenFailing(f: DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] => Unit) = {
    tableFailureFunction = f
    this
  }

  /**
   * function to execute on each row
   */  
  var function : Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = _

  /**
   * Datatable constructor with an empty header
   */  
  def this(rows: List[DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]) = this(new TableHeader(Nil), rows, false)

  /**
   * Adds a new datarow to the existing table
   */  
  def |(r: DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = DataTable(header, rows:::List({r.header_=(header); r}), shouldExecute) 

  /**
   * Adds a new datarow to the existing table and sets the table for immediate execution
   */  
  def |>(r: AbstractDataRow) = { this.|(r); shouldExecute = true; this }

  /**
   * closes the table
   */  
  def | = this 

  /**
   * executes the function on each table row  if the function exists
   */  
  def execute = {
    reset()
    if (function != null) { 
      function()
    }
    if (!isOk) failureFunction(this)
    this
  }

  /**
   * @returns the result of the execution of the table
   */  
  def results: String = {
    val result = 
     header.toString + "\n" +
     rows.map(_.result).mkString("\n")
    
    def alignRows = " " + result.replaceAll("\n\\|", "\n \\|") 
    if (!isOk) 
      alignRows   
    else 
      result
  }

  /**
   * @returns a string representation of the table
   */  
  override def toString = (header.toString + "\n" + rows.mkString("\n"))
  /**
   * @returns an html representation of the table
   */  
  def toHtml = <table class="dataTable">{header.toHtml}{rows map(_.toHtml)}</table>
  
  /**
   * execute a row by checking the execution of the user-supplied function and
   * either displaying the row or displaying the row and an error message
   */  
  private def executeRow(row: AbstractDataRow, value: => Any) = {
    try { 
      value 
    } 
    catch {
      case f: FailureException => row.addFailure(f)
      case s: SkippedException => row.addSkipped(s)
      case e: Throwable => row.addError(e)
    }
  }
  /**
   * apply a function of one argument to the table and set the table for execution
   */  
  def |>[R](f: Function1[T0, R]) = {shouldExecute = true; this.|(f)}

  /**
   * apply a function of one argument to the table
   */  
  def |[R](f: Function1[T0, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function2[T0, T1, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function2[T0, T1, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function3[T0, T1, T2, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function3[T0, T1, T2, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function4[T0, T1, T2, T3, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function4[T0, T1, T2, T3, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function5[T0, T1, T2, T3, T4, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function5[T0, T1, T2, T3, T4, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function6[T0, T1, T2, T3, T4, T5, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function6[T0, T1, T2, T3, T4, T5, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function7[T0, T1, T2, T3, T4, T5, T6, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function7[T0, T1, T2, T3, T4, T5, T6, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18, r.values._19))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]) = {
    
    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18, r.values._19, r.values._20))}; outer }    }
    if (shouldExecute) execute else this
  }
}
case class DataRow1[T0](v0: T0) extends DataRow((v0, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow2[T0, T](v0, v)
}
case class DataRow2[T0, T1](v0: T0, v1: T1) extends DataRow((v0, v1, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow3[T0, T1, T](v0, v1, v)
}
case class DataRow3[T0, T1, T2](v0: T0, v1: T1, v2: T2) extends DataRow((v0, v1, v2, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow4[T0, T1, T2, T](v0, v1, v2, v)
}
case class DataRow4[T0, T1, T2, T3](v0: T0, v1: T1, v2: T2, v3: T3) extends DataRow((v0, v1, v2, v3, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow5[T0, T1, T2, T3, T](v0, v1, v2, v3, v)
}
case class DataRow5[T0, T1, T2, T3, T4](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4) extends DataRow((v0, v1, v2, v3, v4, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow6[T0, T1, T2, T3, T4, T](v0, v1, v2, v3, v4, v)
}
case class DataRow6[T0, T1, T2, T3, T4, T5](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) extends DataRow((v0, v1, v2, v3, v4, v5, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow7[T0, T1, T2, T3, T4, T5, T](v0, v1, v2, v3, v4, v5, v)
}
case class DataRow7[T0, T1, T2, T3, T4, T5, T6](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) extends DataRow((v0, v1, v2, v3, v4, v5, v6, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow8[T0, T1, T2, T3, T4, T5, T6, T](v0, v1, v2, v3, v4, v5, v6, v)
}
case class DataRow8[T0, T1, T2, T3, T4, T5, T6, T7](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow9[T0, T1, T2, T3, T4, T5, T6, T7, T](v0, v1, v2, v3, v4, v5, v6, v7, v)
}
case class DataRow9[T0, T1, T2, T3, T4, T5, T6, T7, T8](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v)
}
case class DataRow10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v)
}
case class DataRow11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v)
}
case class DataRow12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v)
}
case class DataRow13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v)
}
case class DataRow14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v)
}
case class DataRow15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, None, None, None, None, None)) {
  def ![T](v: T) = DataRow16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v)
}
case class DataRow16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, None, None, None, None)) {
  def ![T](v: T) = DataRow17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v)
}
case class DataRow17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, None, None, None)) {
  def ![T](v: T) = DataRow18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v)
}
case class DataRow18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, None, None)) {
  def ![T](v: T) = DataRow19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v)
}
case class DataRow19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, None)) {
  def ![T](v: T) = DataRow20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v)
}
case class DataRow20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19)) {
  
}
/**
 * Extension of a FailureException to allow a better display of this kind of failure (used in HtmlRunner)
 */
case class DataTableFailureException[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](val table: DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) extends FailureException(table.results)

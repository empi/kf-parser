package org.specs.runner
import org.specs.io._
import org.specs.util._
import org.specs.log._
import org.specs._
import scala.xml.{Elem, PrettyPrinter}
import org.specs.specification._
import org.specs.ExtendedThrowable._
import scala.xml.{Elem, PrettyPrinter}

/**
 * Concrete class for the Xml trait. It allows to select a specification to run and an output path
 * Usage: <code>object runner extends XmlRunner("./results/specs", mySpec)</code>
 *
 * The name of the generated file is specification.name by default but can be overriden:<pre>
 * object runner extends XmlRunner("./results/specs", mySpec){ override def fileName="spec-report.xml" }</pre>
 */
case class XmlRunner(val specs: Seq[Specification], outputDirPath: String, fName: Specification => String) extends 
  FileReporter(outputDirPath, fName) with Xml {
  
  /** 
   * Alternative constructor with no specific specifications and default values.
   * This runner can report specifications using the report method.
   * This capacity is used in conjunction with the Runner class:<pre>
   * object runner extends Runner(spec1 :: spec2, HtmlRunner() :: XmlRunner())
   * </pre>
   */
  def this() = this(Nil, "./", XmlNamingFunction.default)
  
  /** alternate constructor with the specification only. The output dir is the current directory */
  def this(specifications: Specification*) = this(specifications, ".", XmlNamingFunction.default)

  /** alternate constructor with one specification only. The output dir is the current directory */
  def this(spec: Specification, outputDirPath: String) = this(List(spec), outputDirPath, XmlNamingFunction.default)
  
  /** alternate constructor with one specification only. */
  def this(spec: Specification, outputDirPath: String, fName: Specification => String) = this(List(spec), outputDirPath, fName)

  /** definition of the file name of a specification. */
  override def fileName(spec: Specification): String = fName(spec) 

  /** definition of the output directory of the report. */
  override def outputDir = normalize(outputDirPath)
}
/**
 * The XmlSuite is the same class as XmlRunner but can be extended with the JUnit trait.
 */
case class XmlSuite(val specs: Seq[Specification], outputDirPath: String, fName: Specification => String) extends 
  FileSuite(outputDirPath, fName) with Xml {
  
  /** 
   * Alternative constructor with no specific specifications and default values.
   * This runner can report specifications using the report method.
   * This capacity is used in conjunction with the Runner class:<pre>
   * object runner extends Runner(spec1 :: spec2, HtmlSuite() :: XmlSuite())
   * </pre>
   */
  def this() = this(Nil, "./", XmlNamingFunction.default)
  
  /** alternate constructor with the specification only. The output dir is the current directory */
  def this(specifications: Specification*) = this(specifications, ".", XmlNamingFunction.default)

  /** alternate constructor with one specification only. The output dir is the current directory */
  def this(spec: Specification, outputDirPath: String) = this(List(spec), outputDirPath, XmlNamingFunction.default)
  
  /** alternate constructor with one specification only. */
  def this(spec: Specification, outputDirPath: String, fName: Specification => String) = this(List(spec), outputDirPath, fName)

  /** definition of the file name of a specification. */
  override def fileName(spec: Specification): String = fName(spec) 

  /** definition of the output directory of the report. */
  override def outputDir = normalize(outputDirPath)
}
/**
 * The <code>Xml</code> trait is used to create an xml file, in a specified output directory
 * with the results of a specification execution.
 * 
 * If the output directory is not specified <pre>object runner extends XmlRunner(mySpec)</pre> then the
 * current directory will be used
 *
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with Xml
 * <code>
 * 
 * The output directory can be overriden if necessary:<pre>
 * class mySpecRunner extends Runner(mySpec) with Xml { override def outputDir = "./results/specs" }</pre>
 */
trait Xml extends File {
  /** definition of the file name of a specification. */
  override def fileName(spec: Specification): String = XmlNamingFunction.default(spec) 

  /** definition of the output directory of the report. */
  override def outputDir = "."
  
  /** definition of xml output. */
  def specOutput(spec: Specification): String = new PrettyPrinter(200, 2).format(asXml(spec))

  /**
   * @returns the specification results translated as to xml (including subspecifications)
   */
  def asXml(s: Specification): Elem = {
    <spec name={s.name} description={s.description} expectations={s.expectationsNb.toString} failures={s.failures.size.toString} errors={s.errors.size.toString}>
      {s.subSpecifications map (asXml(_))}
      {s.systems map (asXml(_))}
    </spec>
  }

  /**
   * @returns the sus results translated as to xml 
   */
  def asXml(sus: Sus): Elem = 
    <sus description={sus.description} expectations={sus.expectationsNb.toString} failures={sus.failures.size.toString} errors={sus.errors.size.toString}>
      {sus.examples map (asXml(_))}
    </sus>

  /**
   * @returns the example results translated as to xml (including sub-examples) 
   */
  def asXml(e: Example): Elem = 
    <example description={e.description} expectations={e.expectationsNb.toString} failures={e.failures.size.toString} errors={e.errors.size.toString}>
     { e.failures map (asXml(_)) }
     { e.skipped map (asXml(_)) }
     { e.errors map (asXml(_)) }
     { e.subExamples map (asXml(_)) }
    </example>

  /**
   * @returns an error translated as to xml 
   */
  def asXml(error: Throwable): Elem = <error location={error.location}>{error.getMessage}</error>

  /**
   * @returns a failure translated as to xml 
   */
  def asXml(failure: FailureException): Elem = <failure location={failure.location}>{failure.message}</failure>

  /**
   * @returns a skipped example translated as to xml 
   */
  def asXml(skipped: SkippedException): Elem = <skipped location={skipped.location}>{skipped.message}</skipped>
}
object XmlNamingFunction {
  val default = { (s: Specification) => NamingFunction.default(s) + ".xml" } 
  val short = { (s: Specification) => NamingFunction.short(s) + ".xml" } 
}

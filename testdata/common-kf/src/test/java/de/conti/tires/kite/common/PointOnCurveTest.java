package de.conti.tires.kite.common;

import nxopen.RuleManager.RuleType;

import de.conti.tires.kite.nxunit.NxTestCase;
import de.conti.tires.kite.nxunit.NxTestHelper;
import de.conti.tires.util.resource.RsException;


/**
 * Testcase for the kf com_isPointOnCurve function.
 * 
 * @author uh05908
 */
public class PointOnCurveTest extends NxTestCase
{

   /**
    * {@inheritDoc}
    */
   @Override
   public void setUp() throws RsException
   {
      super.setUp();
      helper.addSearchPath("FUNC", false);
   }

   /**
    * Tests the kf function "com_isPointOnCurve".
    * 
    * @throws RsException if an NX error occurs
    */
   public void testPointOnLine() throws RsException
   {
      // Create line with p1(0,0,0) and p2 (1,1,1)
      String reference = this.helper.createChildRule("ug_line");
      // Evaluate p(0,0,0)
      String result = this.helper.createAttribute(RuleType.BOOLEAN, NxTestHelper.ROOT, "com_isPointOnCurve(" + reference + ", point(0,0,0))");
      assertTrue(this.helper.evaluateAttribute(result, Boolean.class));

      // Evaluate p(1,1,1)
      result = this.helper.createAttribute(RuleType.BOOLEAN, NxTestHelper.ROOT, "com_isPointOnCurve(" + reference + ", point(1,1,1))");
      assertTrue(this.helper.evaluateAttribute(result, Boolean.class));

      // Evaluate point on line
      result = this.helper.createAttribute(RuleType.BOOLEAN, NxTestHelper.ROOT, "com_isPointOnCurve(" + reference
            + ", point( 0.34096405368207, 0.34096405368207, 0.34096405368207))");
      assertTrue(this.helper.evaluateAttribute(result, boolean.class));
   }

   /**
    * Tests the kf function "com_isPointOnCurve".
    * 
    * @throws RsException if an NX error occurs
    */
   public void testPointNotOnLine() throws RsException
   {
      // Create line with p1(0,0,0) and p2 (1,1,1)
      String reference = this.helper.createChildRule("ug_line");

      // Evaluate something close
      String result = this.helper.createAttribute(RuleType.BOOLEAN, NxTestHelper.ROOT, "com_isPointOnCurve(" + reference
            + ", point(0.47226291732995, 0.52781274046356, 0))");
      assertFalse(this.helper.evaluateAttribute(result, Boolean.class));

      // Evaluate wrong z layer
      result = this.helper.createAttribute(RuleType.BOOLEAN, NxTestHelper.ROOT, "com_isPointOnCurve(" + reference + ", point(0, 0, 0.1))");
      assertFalse(this.helper.evaluateAttribute(result, Boolean.class));
   }
}

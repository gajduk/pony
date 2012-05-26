package cern.colt.matrix.tdouble.algo.solver;

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleILU;

/**
 * Test of DoubleBiCG with ILU
 */
public class DoubleBiCGILUTest extends DoubleBiCGTest {

    public DoubleBiCGILUTest(String arg0) {
        super(arg0);
    }

    protected void createSolver() throws Exception {
        super.createSolver();
        M = new DoubleILU(A.rows());
    }

}

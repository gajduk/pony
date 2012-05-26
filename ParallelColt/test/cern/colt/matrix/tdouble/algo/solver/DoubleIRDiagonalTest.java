package cern.colt.matrix.tdouble.algo.solver;

import cern.colt.matrix.tdouble.algo.solver.preconditioner.DoubleDiagonal;

/**
 * Test of DoubleIR with diagonal preconditioner
 */
public class DoubleIRDiagonalTest extends DoubleGMRESTest {

    public DoubleIRDiagonalTest(String arg0) {
        super(arg0);
    }

    protected void createSolver() throws Exception {
        super.createSolver();
        M = new DoubleDiagonal(A.rows());
    }

}

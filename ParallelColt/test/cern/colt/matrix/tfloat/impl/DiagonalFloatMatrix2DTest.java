package cern.colt.matrix.tfloat.impl;

import cern.colt.list.tfloat.FloatArrayList;
import cern.colt.list.tint.IntArrayList;
import cern.colt.matrix.tfloat.FloatMatrix1D;
import cern.colt.matrix.tfloat.FloatMatrix1DProcedure;
import cern.colt.matrix.tfloat.FloatMatrix2D;
import cern.colt.matrix.tfloat.FloatMatrix2DTest;
import cern.jet.math.tfloat.FloatFunctions;
import edu.emory.mathcs.utils.ConcurrencyUtils;

public class DiagonalFloatMatrix2DTest extends FloatMatrix2DTest {

    protected int DLENGTH;

    protected int DINDEX;

    public DiagonalFloatMatrix2DTest(String arg0) {
        super(arg0);
    }

    protected void createMatrices() throws Exception {
        DINDEX = 3;
        A = new DiagonalFloatMatrix2D(NROWS, NCOLUMNS, DINDEX);
        B = new DiagonalFloatMatrix2D(NROWS, NCOLUMNS, DINDEX);
        Bt = new DiagonalFloatMatrix2D(NCOLUMNS, NROWS, -DINDEX);
        DLENGTH = ((DiagonalFloatMatrix2D) A).diagonalLength();

    }

    protected void populateMatrices() {
        ConcurrencyUtils.setThreadsBeginN_2D(1);
        if (DINDEX >= 0) {
            for (int r = 0; r < DLENGTH; r++) {
                A.setQuick(r, r + DINDEX, (float) Math.random());
            }

            for (int r = 0; r < DLENGTH; r++) {
                B.setQuick(r, r + DINDEX, (float) Math.random());
            }

            for (int r = 0; r < DLENGTH; r++) {
                Bt.setQuick(r - DINDEX, r, (float) Math.random());
            }

        } else {
            for (int r = 0; r < DLENGTH; r++) {
                A.setQuick(r - DINDEX, r, (float) Math.random());
            }

            for (int r = 0; r < DLENGTH; r++) {
                B.setQuick(r - DINDEX, r, (float) Math.random());
            }
            for (int r = 0; r < DLENGTH; r++) {
                Bt.setQuick(r, r + DINDEX, (float) Math.random());
            }

        }
    }

    public void testAssignFloat() {
        float value = (float) Math.random();
        A.assign(value);
        if (DINDEX >= 0) {
            for (int r = 0; r < DLENGTH; r++) {
                assertEquals(value, A.getQuick(r, r + DINDEX), TOL);
            }
        } else {
            for (int r = 0; r < DLENGTH; r++) {
                assertEquals(value, A.getQuick(r - DINDEX, r), TOL);
            }
        }
    }

    public void testAssignFloatArrayArray() {
        float[][] expected = new float[NROWS][NCOLUMNS];
        for (int r = 0; r < NROWS; r++) {
            for (int c = 0; c < NCOLUMNS; c++) {
                expected[r][c] = (float) Math.random();
            }
        }
        A.assign(expected);
        if (DINDEX >= 0) {
            for (int r = 0; r < DLENGTH; r++) {
                assertEquals(expected[r][r + DINDEX], A.getQuick(r, r + DINDEX), TOL);
            }
        } else {
            for (int r = 0; r < DLENGTH; r++) {
                assertEquals(expected[r - DINDEX][r], A.getQuick(r - DINDEX, r), TOL);
            }
        }
    }

    public void testAssignFloatFunction() {
        FloatMatrix2D Acopy = A.copy();
        A.assign(FloatFunctions.acos);
        if (DINDEX >= 0) {
            for (int r = 0; r < DLENGTH; r++) {
                float expected = (float) Math.acos(Acopy.getQuick(r, r + DINDEX));
                assertEquals(expected, A.getQuick(r, r + DINDEX), TOL);
            }
        } else {
            for (int r = 0; r < DLENGTH; r++) {
                float expected = (float) Math.acos(Acopy.getQuick(r - DINDEX, r));
                assertEquals(expected, A.getQuick(r - DINDEX, r), TOL);
            }
        }
    }

    public void testAssignFloatMatrix2DFloatFloatFunction() {
        FloatMatrix2D Acopy = A.copy();
        A.assign(B, FloatFunctions.div);
        if (DINDEX >= 0) {
            for (int r = 0; r < DLENGTH; r++) {
                assertEquals(Acopy.getQuick(r, r + DINDEX) / B.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX), TOL);
            }
        } else {
            for (int r = 0; r < DLENGTH; r++) {
                assertEquals(Acopy.getQuick(r - DINDEX, r) / B.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX, r), TOL);
            }
        }
    }

    public void testAssignFloatMatrix2DFloatFloatFunctionIntArrayListIntArrayList() {
        IntArrayList rowList = new IntArrayList();
        IntArrayList columnList = new IntArrayList();
        if (DINDEX >= 0) {
            for (int r = 0; r < DLENGTH; r++) {
                rowList.add(r);
                columnList.add(r + DINDEX);
            }
            FloatMatrix2D Acopy = A.copy();
            A.assign(B, FloatFunctions.div, rowList, columnList);
            for (int r = 0; r < DLENGTH; r++) {
                assertEquals(Acopy.getQuick(r, r + DINDEX) / B.getQuick(r, r + DINDEX), A.getQuick(r, r + DINDEX), TOL);
            }
        } else {
            for (int r = 0; r < DLENGTH; r++) {
                rowList.add(r - DINDEX);
                columnList.add(r);
            }
            FloatMatrix2D Acopy = A.copy();
            A.assign(B, FloatFunctions.div, rowList, columnList);
            for (int r = 0; r < DLENGTH; r++) {
                assertEquals(Acopy.getQuick(r - DINDEX, r) / B.getQuick(r - DINDEX, r), A.getQuick(r - DINDEX, r), TOL);
            }
        }
    }

    public void testCardinality() {
        int card = A.cardinality();
        assertEquals(DLENGTH, card);
    }

    public void testMaxLocation() {
        A.assign(0);
        if (DINDEX >= 0) {
            A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 0.7f);
            A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 0.1f);
            float[] maxAndLoc = A.getMaxLocation();
            assertEquals(0.7, maxAndLoc[0], TOL);
            assertEquals(NROWS / 3, (int) maxAndLoc[1]);
            assertEquals(NROWS / 3 + DINDEX, (int) maxAndLoc[2]);
        } else {
            A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 0.7f);
            A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 0.1f);
            float[] maxAndLoc = A.getMaxLocation();
            assertEquals(0.7, maxAndLoc[0], TOL);
            assertEquals(NROWS / 3 - DINDEX, (int) maxAndLoc[1]);
            assertEquals(NROWS / 3, (int) maxAndLoc[2]);
        }
    }

    public void testMinLocation() {
        A.assign(0);
        if (DINDEX >= 0) {
            A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, -0.7f);
            A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, -0.1f);
            float[] minAndLoc = A.getMinLocation();
            assertEquals(-0.7, minAndLoc[0], TOL);
            assertEquals(NROWS / 3, (int) minAndLoc[1]);
            assertEquals(NROWS / 3 + DINDEX, (int) minAndLoc[2]);
        } else {
            A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, -0.7f);
            A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, -0.1f);
            float[] minAndLoc = A.getMinLocation();
            assertEquals(-0.7, minAndLoc[0], TOL);
            assertEquals(NROWS / 3 - DINDEX, (int) minAndLoc[1]);
            assertEquals(NROWS / 3, (int) minAndLoc[2]);
        }
    }

    public void testGetNegativeValues() {
        A.assign(0);
        if (DINDEX >= 0) {
            A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, -0.7f);
            A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, -0.1f);
            IntArrayList rowList = new IntArrayList();
            IntArrayList columnList = new IntArrayList();
            FloatArrayList valueList = new FloatArrayList();
            A.getNegativeValues(rowList, columnList, valueList);
            assertEquals(2, rowList.size());
            assertEquals(2, columnList.size());
            assertEquals(2, valueList.size());
            assertTrue(rowList.contains(NROWS / 3));
            assertTrue(rowList.contains(NROWS / 2));
            assertTrue(columnList.contains(NROWS / 3 + DINDEX));
            assertTrue(columnList.contains(NROWS / 2 + DINDEX));
            assertTrue(valueList.contains(-0.7f));
            assertTrue(valueList.contains(-0.1f));
        } else {
            A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, -0.7f);
            A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, -0.1f);
            IntArrayList rowList = new IntArrayList();
            IntArrayList columnList = new IntArrayList();
            FloatArrayList valueList = new FloatArrayList();
            A.getNegativeValues(rowList, columnList, valueList);
            assertEquals(2, rowList.size());
            assertEquals(2, columnList.size());
            assertEquals(2, valueList.size());
            assertTrue(rowList.contains(NROWS / 3 - DINDEX));
            assertTrue(rowList.contains(NROWS / 2 - DINDEX));
            assertTrue(columnList.contains(NROWS / 3));
            assertTrue(columnList.contains(NROWS / 2));
            assertTrue(valueList.contains(-0.7f));
            assertTrue(valueList.contains(-0.1f));
        }
    }

    public void testGetNonZeros() {
        A.assign(0);
        if (DINDEX >= 0) {
            A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 0.7f);
            A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 0.1f);
            IntArrayList rowList = new IntArrayList();
            IntArrayList columnList = new IntArrayList();
            FloatArrayList valueList = new FloatArrayList();
            A.getNonZeros(rowList, columnList, valueList);
            assertEquals(2, rowList.size());
            assertEquals(2, columnList.size());
            assertEquals(2, valueList.size());
            assertTrue(rowList.contains(NROWS / 3));
            assertTrue(rowList.contains(NROWS / 2));
            assertTrue(columnList.contains(NROWS / 3 + DINDEX));
            assertTrue(columnList.contains(NROWS / 2 + DINDEX));
            assertTrue(valueList.contains(0.7f));
            assertTrue(valueList.contains(0.1f));
        } else {
            A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 0.7f);
            A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 0.1f);
            IntArrayList rowList = new IntArrayList();
            IntArrayList columnList = new IntArrayList();
            FloatArrayList valueList = new FloatArrayList();
            A.getNonZeros(rowList, columnList, valueList);
            assertEquals(2, rowList.size());
            assertEquals(2, columnList.size());
            assertEquals(2, valueList.size());
            assertTrue(rowList.contains(NROWS / 3 - DINDEX));
            assertTrue(rowList.contains(NROWS / 2 - DINDEX));
            assertTrue(columnList.contains(NROWS / 3));
            assertTrue(columnList.contains(NROWS / 2));
            assertTrue(valueList.contains(0.7f));
            assertTrue(valueList.contains(0.1f));
        }
    }

    public void testGetPositiveValues() {
        A.assign(0);
        if (DINDEX >= 0) {
            A.setQuick(NROWS / 3, NROWS / 3 + DINDEX, 0.7f);
            A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, 0.1f);
            IntArrayList rowList = new IntArrayList();
            IntArrayList columnList = new IntArrayList();
            FloatArrayList valueList = new FloatArrayList();
            A.getPositiveValues(rowList, columnList, valueList);
            assertEquals(2, rowList.size());
            assertEquals(2, columnList.size());
            assertEquals(2, valueList.size());
            assertTrue(rowList.contains(NROWS / 3));
            assertTrue(rowList.contains(NROWS / 2));
            assertTrue(columnList.contains(NROWS / 3 + DINDEX));
            assertTrue(columnList.contains(NROWS / 2 + DINDEX));
            assertTrue(valueList.contains(0.7f));
            assertTrue(valueList.contains(0.1f));
        } else {
            A.setQuick(NROWS / 3 - DINDEX, NROWS / 3, 0.7f);
            A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, 0.1f);
            IntArrayList rowList = new IntArrayList();
            IntArrayList columnList = new IntArrayList();
            FloatArrayList valueList = new FloatArrayList();
            A.getPositiveValues(rowList, columnList, valueList);
            assertEquals(2, rowList.size());
            assertEquals(2, columnList.size());
            assertEquals(2, valueList.size());
            assertTrue(rowList.contains(NROWS / 3 - DINDEX));
            assertTrue(rowList.contains(NROWS / 2 - DINDEX));
            assertTrue(columnList.contains(NROWS / 3));
            assertTrue(columnList.contains(NROWS / 2));
            assertTrue(valueList.contains(0.7f));
            assertTrue(valueList.contains(0.1f));
        }
    }

    public void testToArray() {
        float[][] array = A.toArray();
        assertTrue(NROWS == array.length);
        for (int r = 0; r < NROWS; r++) {
            assertTrue(NCOLUMNS == array[r].length);
            for (int c = 0; c < NCOLUMNS; c++) {
                assertEquals(array[r][c], A.getQuick(r, c), TOL);
            }
        }
    }

    public void testVectorize() {
        FloatMatrix1D Avec = A.vectorize();
        int idx = 0;
        for (int c = 0; c < NCOLUMNS; c++) {
            for (int r = 0; r < NROWS; r++) {
                assertEquals(A.getQuick(r, c), Avec.getQuick(idx++), TOL);
            }
        }
    }

    public void testViewColumn() {
        FloatMatrix1D col = A.viewColumn(NCOLUMNS / 2);
        assertEquals(NROWS, col.size());
        for (int r = 0; r < NROWS; r++) {
            assertEquals(A.getQuick(r, NCOLUMNS / 2), col.getQuick(r), TOL);
        }
    }

    public void testViewColumnFlip() {
        FloatMatrix2D B = A.viewColumnFlip();
        assertEquals(A.size(), B.size());
        for (int r = 0; r < NROWS; r++) {
            for (int c = 0; c < NCOLUMNS; c++) {
                assertEquals(A.getQuick(r, NCOLUMNS - 1 - c), B.getQuick(r, c), TOL);
            }
        }
    }

    public void testViewDice() {
        FloatMatrix2D B = A.viewDice();
        assertEquals(NROWS, B.columns());
        assertEquals(NCOLUMNS, B.rows());
        for (int r = 0; r < NROWS; r++) {
            for (int c = 0; c < NCOLUMNS; c++) {
                assertEquals(A.getQuick(r, c), B.getQuick(c, r), TOL);
            }
        }
    }

    public void testViewPart() {
        FloatMatrix2D B = A.viewPart(NROWS / 2, NCOLUMNS / 2, NROWS / 3, NCOLUMNS / 3);
        assertEquals(NROWS / 3, B.rows());
        assertEquals(NCOLUMNS / 3, B.columns());
        for (int r = 0; r < NROWS / 3; r++) {
            for (int c = 0; c < NCOLUMNS / 3; c++) {
                assertEquals(A.getQuick(NROWS / 2 + r, NCOLUMNS / 2 + c), B.getQuick(r, c), TOL);
            }
        }
    }

    public void testViewRow() {
        FloatMatrix1D B = A.viewRow(NROWS / 2);
        assertEquals(NCOLUMNS, B.size());
        for (int r = 0; r < NCOLUMNS; r++) {
            assertEquals(A.getQuick(NROWS / 2, r), B.getQuick(r), TOL);
        }
    }

    public void testViewRowFlip() {
        FloatMatrix2D B = A.viewRowFlip();
        assertEquals(A.size(), B.size());
        for (int r = 0; r < NROWS; r++) {
            for (int c = 0; c < NCOLUMNS; c++) {
                assertEquals(A.getQuick(NROWS - 1 - r, c), B.getQuick(r, c), TOL);
            }
        }
    }

    public void testViewSelectionFloatMatrix1DProcedure() {
        final float value = 2;
        A.assign(0);
        if (DINDEX >= 0) {
            A.setQuick(NROWS / 4, NROWS / 4 + DINDEX, value);
            A.setQuick(NROWS / 2, NROWS / 2 + DINDEX, value);
            FloatMatrix2D B = A.viewSelection(new FloatMatrix1DProcedure() {
                public boolean apply(FloatMatrix1D element) {
                    if (Math.abs(element.getQuick(NROWS / 4 + DINDEX) - value) < TOL) {
                        return true;
                    } else {
                        return false;
                    }
                }
            });
            assertEquals(1, B.rows());
            assertEquals(NCOLUMNS, B.columns());
            assertEquals(A.getQuick(NROWS / 4, NROWS / 4 + DINDEX), B.getQuick(0, NROWS / 4 + DINDEX), TOL);
        } else {
            A.setQuick(NROWS / 4 - DINDEX, NROWS / 4, value);
            A.setQuick(NROWS / 2 - DINDEX, NROWS / 2, value);
            FloatMatrix2D B = A.viewSelection(new FloatMatrix1DProcedure() {
                public boolean apply(FloatMatrix1D element) {
                    if (Math.abs(element.getQuick(NROWS / 4) - value) < TOL) {
                        return true;
                    } else {
                        return false;
                    }
                }
            });
            assertEquals(1, B.rows());
            assertEquals(NCOLUMNS, B.columns());
            assertEquals(A.getQuick(NROWS / 4 - DINDEX, NROWS / 4), B.getQuick(0, NROWS / 4), TOL);
        }
    }

    public void testViewSelectionIntArrayIntArray() {
        int[] rowIndexes = new int[] { NROWS / 6, NROWS / 5, NROWS / 4, NROWS / 3, NROWS / 2 };
        int[] colIndexes = new int[] { NROWS / 6, NROWS / 5, NROWS / 4, NROWS / 3, NROWS / 2, NROWS - 1 };
        FloatMatrix2D B = A.viewSelection(rowIndexes, colIndexes);
        assertEquals(rowIndexes.length, B.rows());
        assertEquals(colIndexes.length, B.columns());
        for (int r = 0; r < rowIndexes.length; r++) {
            for (int c = 0; c < colIndexes.length; c++) {
                assertEquals(A.getQuick(rowIndexes[r], colIndexes[c]), B.getQuick(r, c), TOL);
            }
        }
    }

    public void testViewSorted() {
        FloatMatrix2D B = A.viewSorted(1);
        for (int r = 0; r < NROWS - 1; r++) {
            assertTrue(B.getQuick(r + 1, 1) >= B.getQuick(r, 1));
        }
    }

    public void testViewStrides() {
        int rowStride = 3;
        int colStride = 5;
        FloatMatrix2D B = A.viewStrides(rowStride, colStride);
        for (int r = 0; r < B.rows(); r++) {
            for (int c = 0; c < B.columns(); c++) {
                assertEquals(A.getQuick(r * rowStride, c * colStride), B.getQuick(r, c), TOL);
            }
        }
    }

    public void testZMultFloatMatrix2DFloatMatrix2DFloatFloatBooleanBoolean() {
        float alpha = 3;
        float beta = 5;
        FloatMatrix2D C = new DiagonalFloatMatrix2D(NROWS, NROWS, 0);
        for (int i = 0; i < DLENGTH; i++) {
            C.setQuick(i, i, (float) Math.random());
        }
        float[][] expected = C.toArray();
        C = A.zMult(Bt, C, alpha, beta, false, false);
        for (int j = 0; j < NROWS; j++) {
            for (int i = 0; i < NROWS; i++) {
                float s = 0;
                for (int k = 0; k < NCOLUMNS; k++) {
                    s += A.getQuick(i, k) * Bt.getQuick(k, j);
                }
                expected[i][j] = s * alpha + expected[i][j] * beta;
            }
        }
        for (int r = 0; r < NROWS; r++) {
            for (int c = 0; c < NROWS; c++) {
                assertEquals(expected[r][c], C.getQuick(r, c), TOL);
            }
        }

        //---
        C = null;
        C = A.zMult(Bt, C, alpha, beta, false, false);
        expected = new float[NROWS][NROWS];
        for (int j = 0; j < NROWS; j++) {
            for (int i = 0; i < NROWS; i++) {
                float s = 0;
                for (int k = 0; k < NCOLUMNS; k++) {
                    s += A.getQuick(i, k) * Bt.getQuick(k, j);
                }
                expected[i][j] = s * alpha;
            }
        }
        for (int r = 0; r < NROWS; r++) {
            for (int c = 0; c < NROWS; c++) {
                assertEquals(expected[r][c], C.getQuick(r, c), TOL);
            }
        }

        //transposeA
        C = new DiagonalFloatMatrix2D(NCOLUMNS, NCOLUMNS, 0);
        for (int i = 0; i < DLENGTH; i++) {
            C.setQuick(i, i, (float) Math.random());
        }
        expected = C.toArray();
        C = A.zMult(B, C, alpha, beta, true, false);
        for (int j = 0; j < NCOLUMNS; j++) {
            for (int i = 0; i < NCOLUMNS; i++) {
                float s = 0;
                for (int k = 0; k < NROWS; k++) {
                    s += A.getQuick(k, i) * B.getQuick(k, j);
                }
                expected[i][j] = s * alpha + expected[i][j] * beta;
            }
        }
        for (int r = 0; r < NCOLUMNS; r++) {
            for (int c = 0; c < NCOLUMNS; c++) {
                assertEquals(expected[r][c], C.getQuick(r, c), TOL);
            }
        }
        //---
        C = null;
        C = A.zMult(B, C, alpha, beta, true, false);
        expected = new float[NCOLUMNS][NCOLUMNS];
        for (int j = 0; j < NCOLUMNS; j++) {
            for (int i = 0; i < NCOLUMNS; i++) {
                float s = 0;
                for (int k = 0; k < NROWS; k++) {
                    s += A.getQuick(k, i) * B.getQuick(k, j);
                }
                expected[i][j] = s * alpha;
            }
        }
        for (int r = 0; r < NCOLUMNS; r++) {
            for (int c = 0; c < NCOLUMNS; c++) {
                assertEquals(expected[r][c], C.getQuick(r, c), TOL);
            }
        }

        //transposeB
        C = new DiagonalFloatMatrix2D(NROWS, NROWS, 0);
        for (int i = 0; i < DLENGTH; i++) {
            C.setQuick(i, i, (float) Math.random());
        }
        expected = C.toArray();
        C = A.zMult(B, C, alpha, beta, false, true);
        for (int j = 0; j < NROWS; j++) {
            for (int i = 0; i < NROWS; i++) {
                float s = 0;
                for (int k = 0; k < NCOLUMNS; k++) {
                    s += A.getQuick(i, k) * B.getQuick(j, k);
                }
                expected[i][j] = s * alpha + expected[i][j] * beta;
            }
        }
        for (int r = 0; r < NROWS; r++) {
            for (int c = 0; c < NROWS; c++) {
                assertEquals(expected[r][c], C.getQuick(r, c), TOL);
            }
        }
        //---
        C = null;
        C = A.zMult(B, C, alpha, beta, false, true);
        expected = new float[NROWS][NROWS];
        for (int j = 0; j < NROWS; j++) {
            for (int i = 0; i < NROWS; i++) {
                float s = 0;
                for (int k = 0; k < NCOLUMNS; k++) {
                    s += A.getQuick(i, k) * B.getQuick(j, k);
                }
                expected[i][j] = s * alpha;
            }
        }
        for (int r = 0; r < NROWS; r++) {
            for (int c = 0; c < NROWS; c++) {
                assertEquals(expected[r][c], C.getQuick(r, c), TOL);
            }
        }
        //transposeA and transposeB
        C = new DiagonalFloatMatrix2D(NCOLUMNS, NCOLUMNS, 0);
        for (int i = 0; i < DLENGTH; i++) {
            C.setQuick(i, i, (float) Math.random());
        }
        expected = C.toArray();
        C = A.zMult(Bt, C, alpha, beta, true, true);
        for (int j = 0; j < NCOLUMNS; j++) {
            for (int i = 0; i < NCOLUMNS; i++) {
                float s = 0;
                for (int k = 0; k < NROWS; k++) {
                    s += A.getQuick(k, i) * Bt.getQuick(j, k);
                }
                expected[i][j] = s * alpha + expected[i][j] * beta;
            }
        }
        for (int r = 0; r < NCOLUMNS; r++) {
            for (int c = 0; c < NCOLUMNS; c++) {
                assertEquals(expected[r][c], C.getQuick(r, c), TOL);
            }
        }
        //---
        C = null;
        C = A.zMult(Bt, C, alpha, beta, true, true);
        expected = new float[NCOLUMNS][NCOLUMNS];
        for (int j = 0; j < NCOLUMNS; j++) {
            for (int i = 0; i < NCOLUMNS; i++) {
                float s = 0;
                for (int k = 0; k < NROWS; k++) {
                    s += A.getQuick(k, i) * Bt.getQuick(j, k);
                }
                expected[i][j] = s * alpha;
            }
        }
        for (int r = 0; r < NCOLUMNS; r++) {
            for (int c = 0; c < NCOLUMNS; c++) {
                assertEquals(expected[r][c], C.getQuick(r, c), TOL);
            }
        }

    }
}

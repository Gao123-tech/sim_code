#include "acom_fec_ldpc.h"
#include "mex.h"
#define MAX_OUT 2

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 0)
    {
        mexErrMsgTxt("Error input in function encode_M, try again...\n");
    }
    if (nlhs > MAX_OUT)
    {
        mexErrMsgTxt("Too many output arguments, try again...\n");
    }
    plhs[0] = mxCreateNumericMatrix(H_ROW_SIZE, H_COLUMN_SIZE, mxUINT8_CLASS, mxREAL);
    plhs[1] = mxCreateNumericMatrix(H_ROW_SIZE, H_COLUMN_SIZE, mxUINT8_CLASS, mxREAL);
    unsigned char (*H)[H_COLUMN_SIZE] = (unsigned char (*)[H_COLUMN_SIZE])(mxGetPr(plhs[0]));
    unsigned char (*Ht)[H_COLUMN_SIZE] = (unsigned char (*)[H_COLUMN_SIZE])(mxGetPr(plhs[1]));
    produce_qc_H_Ht(H, Ht);
    //plhs[2] = mxCreateNumericMatrix(5, 5, mxUINT8_CLASS, mxREAL);
    //unsigned char (*test)[5] = (unsigned char (*)[5])(mxGetPr(plhs[2]));
    int i = 0, j = 0;
    /* for (i = 0; i < 5; i++) {
        test[i][i] = 1;
    } */
    /* for (i = 0; i < 1200; i++) {
        for (j = 0; j < 2400; j++) {
            printf("%hd", H[i][j]);
        }
        printf("\n");
    } */
}
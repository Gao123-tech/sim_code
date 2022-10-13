#include "mex.h"
#include "acom_fec_ldpc.h"

/*********************************************************************
 * @description: The c interface of ldpc decoding for matlab function
 * @date: 2022.04.30
 * @author: gaorongyu
 * @input:
 * @output:
 *********************************************************************/

//-------------------------------------matlab interface-------------------------------------
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{ // encoding part
    if (nrhs != 2)
    {
        mexErrMsgTxt("Wrong number of input arguments. Try again.\n");
    }
    if (nlhs > 1)
    {
        mexErrMsgTxt("Too many output arguments, try again.\n");
    }
    int m = mxGetM(prhs[0]);
    int n = mxGetN(prhs[0]);                      // get input matrix size
    //printf("get the origin bits matrix size: %d * %d\n", m, n);
    //plhs[0] = mxCreateDoubleMatrix(m, n, mxREAL); // malloc output matrix memory
    char *in_ptr = (char *)mxGetPr(prhs[0]);
    //int *out_ptr = (int *)mxGetPr(plhs[0]); // get output matrix pointer

    int i = 0;
    // for (i = 0; i < n; i++) {
    //     printf("%d", in_ptr[i]);
    // }
    //-------------------------------------------------------
    m = mxGetM(prhs[1]);
    n = mxGetN(prhs[1]);
    //printf("\nget the Ht matrix size %d * %d", m, n);
    unsigned char (*Ht)[H_COLUMN_SIZE] = (unsigned char (*)[H_COLUMN_SIZE])(mxGetPr(prhs[1]));


    //-------------------------------------------------------
    //printf("\n");
    // extern unsigned char H[H_ROW_SIZE][H_COLUMN_SIZE];
    // extern unsigned char Ht[H_ROW_SIZE][H_COLUMN_SIZE];
    //produce_qc_H_Ht(); // initiate the check matrix and the down triangle matrix
    //char info_bytes[128] = {0};
    //unsigned char info_bits[H_COLUMN_SIZE - H_ROW_SIZE] = {0};
    const char *info_bits = in_ptr;
    unsigned char check_bits[H_ROW_SIZE] = {0};
    //unsigned char ldpccodes[1024] = {0};
    //strcpy(info_bytes, in_ptr);
    // int i = 0;
    // for (i = 0; i < 128; i++)
    // {
    //     info_bits[i * 8] = info_bytes[i] & 1;
    //     info_bits[i * 8 + 1] = (info_bytes[i] >> 1) & 1;
    //     info_bits[i * 8 + 2] = (info_bytes[i] >> 2) & 1;
    //     info_bits[i * 8 + 3] = (info_bytes[i] >> 3) & 1;
    //     info_bits[i * 8 + 4] = (info_bytes[i] >> 4) & 1;
    //     info_bits[i * 8 + 5] = (info_bytes[i] >> 5) & 1;
    //     info_bits[i * 8 + 6] = (info_bytes[i] >> 6) & 1;
    //     info_bits[i * 8 + 7] = (info_bytes[i] >> 7) & 1;
    // }
    //-----------------------------decode-------------------------------
    int j = 0;
    const int k = H_ROW_SIZE;
    for (i = 0; i < H_ROW_SIZE; i++)
    {
        unsigned char temp = 0;

        for (j = 0; j < k; j++)
        {
            if (Ht[i][j] == 1)
            {
                temp ^= info_bits[j];
            }
        }
        int l = 0;
        for (l = 0; l < i; l++)
        {
            // check_bits[i]=(check_bits[j]&H[i][j+k])^check_bits[j-1];
            if (Ht[i][l + k] == 1)
                temp ^= check_bits[l];
        }
        if (Ht[i][i + k] == 1)
            check_bits[i] = temp;
        else
        {
            printf("The H_matrix is not inversible at row %d!!!\n", i);
            return;
        }
    }
    plhs[0] = mxCreateNumericMatrix(1, H_COLUMN_SIZE, mxUINT8_CLASS, mxREAL);
    unsigned char *ldpccodes = (unsigned char*)mxGetPr(plhs[0]);
    m = mxGetM(plhs[0]);
    n = mxGetN(plhs[0]); 
    //printf("get the output matrix size: %d * %d", m, n);
    for (i = 0; i < H_COLUMN_SIZE; i++)
    {
        if (i < k)
        {
            ldpccodes[i] = info_bits[i];
            //printf("%d", info_bits[i]);
        }
        else
        {
            ldpccodes[i] = check_bits[i - k];
            //printf("%d", check_bits[i - k]);
        }
    }
    /* plhs[0] = mxCreateCharArray(1, H_COLUMN_SIZE);  //output ldpc codes to matlab terminal
    unsigned char *ldpc_out = (unsigned char*)mxGetPr(plhs[0]);
    memcpy(ldpc_out, ldpccodes, sizeof(ldpccodes));  //force transefer to unsigned char
    plhs[1] = mxCreateCharArray(H_ROW_SIZE, H_COLUMN_SIZE);
    unsigned char (*H_out)[H_COLUMN_SIZE] = (unsigned char (*)[H_COLUMN_SIZE])plhs[1];
    memcmp(H_out, H, sizeof(unsigned char) * H_ROW_SIZE * H_COLUMN_SIZE); */
    // if (stop_con(H, ldpccodes, 1200)) printf("\nsuccessfully encode!\n");
    // else printf("failure in encoding\n");
    //printf("Encode completed!\n");
}

//-------------------------------------------------------------------------------------------
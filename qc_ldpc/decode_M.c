#include "acom_fec_ldpc.h"
#include "mex.h"
#define MAX_OUT 2

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    if (nrhs != 2)
    {
        mexErrMsgTxt("Error input in function encode_M, try again...\n");
    }
    if (nlhs > MAX_OUT)
    {
        mexErrMsgTxt("Too many output arguments, try again...\n");
    }
    //------------------------------------------------------
    int m = mxGetM(prhs[1]);
    int n = mxGetN(prhs[1]);
    //printf("\nget the check matrix size in decode function %d * %d\n", m, n);
    mattype H = (mattype)mxGetPr(prhs[1]);
    int i, j;
    /* for (i = 0; i < 1200; i++) {
        for (j = 0; j < 2400; j++) {
            printf("%hd", H[i][j]);
        }
        printf("\n");
    }
 */
    struct variable_node variable[H_COLUMN_SIZE] = {0}; 
    struct check_node check[H_ROW_SIZE] = {0};  

    //------------------------------------------------------
    ini_matrix(H, variable, check);                   // make check matrix to decode
    //double *codes_in = mxGetPr(prhs[0]); // get demodulated data
    // extern unsigned char H[H_ROW_SIZE][H_COLUMN_SIZE];
    // extern struct check_node check[];
    // extern struct variable_node variable[];
    double VAR_INFO[VAR_NUM][MAX_VAR_LINK] = {{0}};
    double CHECK_INFO[CHECK_NUM][MAX_CHECK_LINK] = {{0}};
    double TOTAL_INFO[VAR_NUM] = {0};
    //double CHANNEL_INFO[VAR_NUM] = {0};
    const double *CHANNEL_INFO = mxGetPr(prhs[0]);
    unsigned char code_dec[H_COLUMN_SIZE];
    plhs[0] = mxCreateNumericMatrix(1, H_COLUMN_SIZE - H_ROW_SIZE, mxUINT8_CLASS, mxREAL);
    unsigned char *decoded_infobits = (unsigned char*)mxGetPr(plhs[0]);
    // ldpc decode process
    initial(VAR_INFO, CHANNEL_INFO, variable);
    //printf("\n");

    //------------------------------------------------------
    soft_decision(CHANNEL_INFO, code_dec, 2400);
    //int i = 0;
    // for (i = 0; i < 100; i++) {
    //     printf("%d", code_dec[i]);
    // }
    // if (stop_con(H, code_dec, CHECK_NUM)) printf("the soft decision is correct before decode\n");
    //---------------------------------------------------------
    int iter = 0;
    while (iter <= MAX_ITER)
    {
        //cal_total_info(TOTAL_INFO, CHECK_INFO, CHANNEL_INFO, variable, check, VAR_NUM);

        //soft_decision(TOTAL_INFO, code_dec, VAR_NUM);

        // if (stop_con(H, code_dec, CHECK_NUM))
        // {
        //     //printf("Successfully decode.\n");
        //     break;
        // }

        check_renew(VAR_INFO, CHECK_INFO, check, variable, CHECK_NUM);

        cal_total_info(TOTAL_INFO, CHECK_INFO, CHANNEL_INFO, variable, check, VAR_NUM);

        var_renew(VAR_INFO, CHECK_INFO, TOTAL_INFO, variable, check, VAR_NUM);

        soft_decision(TOTAL_INFO, code_dec, VAR_NUM);

        if (stop_con(H, code_dec, CHECK_NUM))
        {
            //printf("Successfully decode.\n");
            break;
        }

        iter++;
    }
    // if (iter == MAX_ITER + 1)
    // {
    //     //printf("failure in decoding\n");
    // }
    for (i = 0; i < H_COLUMN_SIZE - H_ROW_SIZE; i++) {
        decoded_infobits[i] = code_dec[i];
    }
    /* // unsigned char code_dec_byte[128] = {0};
    // int i;
    // for (i = 0; i < 128; i++)
    // {
    //     code_dec_byte[i] |= (code_dec[8 * i] << 7);
    //     code_dec_byte[i] |= (code_dec[8 * i + 1] << 6);
    //     code_dec_byte[i] |= (code_dec[8 * i + 2] << 5);
    //     code_dec_byte[i] |= (code_dec[8 * i + 3] << 4);
    //     code_dec_byte[i] |= (code_dec[8 * i + 4] << 3);
    //     code_dec_byte[i] |= (code_dec[8 * i + 5] << 2);
    //     code_dec_byte[i] |= (code_dec[8 * i + 6] << 1);
    //     code_dec_byte[i] |= (code_dec[8 * i + 7]);
    // }
    // plhs[0] = mxCreateNumericMatrix(1, H_ROW_SIZE, mxUINT8_CLASS, mxREAL);
    // unsigned char *code_out = mxGetPr(plhs[0]);
    // memcpy(code_out, code_dec, sizeof(unsigned char) * (H_COLUMN_SIZE - H_ROW_SIZE)); // output the decoded result
    // printf("After decoding, the infomation is recovered as:\n");
    // printf("%s\n", code_dec_byte); */
}

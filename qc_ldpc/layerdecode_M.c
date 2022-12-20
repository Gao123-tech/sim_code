#include "acom_fec_ldpc.h"
#include "mex.h"
//#define PRINT_CHAN_INFO
#define LAYER H_ROW_SIZE
#define MAX_OUT 3

typedef struct decode_info
{
    /* data */
    int iter_cnt;
    bool sucFlag;
    
};

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
    printf("Layer number is set: %d\n", LAYER);
    //int renewOrder[LAYER] = {0};
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
    //double *TOTAL_INFO = mxGetPr(prhs[0]);
    unsigned char code_dec[H_COLUMN_SIZE];
    plhs[0] = mxCreateNumericMatrix(1, H_COLUMN_SIZE - H_ROW_SIZE, mxUINT8_CLASS, mxREAL);
    plhs[1] = mxCreateNumericMatrix(1, 1, mxINT32_CLASS, mxREAL);
    plhs[2] = mxCreateNumericMatrix(1, MAX_ITER + 1, mxDOUBLE_CLASS, mxREAL);
    double *mean_chaninfo = (double*)mxGetPr(plhs[2]);
    int *iter_num = (int *)mxGetPr(plhs[1]);
    *iter_num = MAX_ITER;
    unsigned char *decoded_infobits = (unsigned char*)mxGetPr(plhs[0]);
    // ldpc decode process
    initial(VAR_INFO, CHANNEL_INFO, variable);
    for(i = 0; i < H_COLUMN_SIZE; i++) {
        TOTAL_INFO[i] = CHANNEL_INFO[i];
    }
    //printf("\n");

    //------------------------------------------------------
    //soft_decision(CHANNEL_INFO, code_dec, 2400);
    //int i = 0;
    // for (i = 0; i < 100; i++) {
    //     printf("%d", code_dec[i]);
    // }
    // if (stop_con(H, code_dec, CHECK_NUM)) printf("the soft decision is correct before decode\n");
    //---------------------------------------------------------
    int iter = 0;
    bool suc_flag = false;
//     while (iter <= MAX_ITER)
//     {
//         //cal_total_info(TOTAL_INFO, CHECK_INFO, CHANNEL_INFO, variable, check, VAR_NUM);

//         //soft_decision(TOTAL_INFO, code_dec, VAR_NUM);

//         // if (stop_con(H, code_dec, CHECK_NUM))
//         // {
//         //     //printf("Successfully decode.\n");
//         //     break;
//         // }

//         check_renew(VAR_INFO, CHECK_INFO, check, variable, CHECK_NUM);

//         cal_total_info(TOTAL_INFO, CHECK_INFO, CHANNEL_INFO, variable, check, VAR_NUM);

//         var_renew(VAR_INFO, CHECK_INFO, TOTAL_INFO, variable, check, VAR_NUM);

// #ifdef PRINT_CHAN_INFO
        
//         // for (i = 0; i < H_COLUMN_SIZE / 10; i++) {
//         //     for (j = 0; j < 10; j++) {
//         //         printf("%f ", TOTAL_INFO[i * 10 + j]);
//         //     } 
//         //     printf("\n");
//         // } printf("\n");
//         double temp = 0;
//         for (i = 0; i < H_COLUMN_SIZE; i++) {
//             temp += abs(TOTAL_INFO[i]);
//         }
//         mean_chaninfo[iter] = temp / H_COLUMN_SIZE;

// #endif

//         soft_decision(TOTAL_INFO, code_dec, VAR_NUM);

//         if (stop_con(H, code_dec, CHECK_NUM))
//         {
//             //printf("Successfully decode.\n");
//             *iter_num = iter;
//             suc_flag = true;
//             break;
//         }

//         iter++;
//     }
    while (iter <= MAX_ITER)
    {
        /* code */
        int laynum = 0;
        for (laynum = 0; laynum < LAYER; laynum++) {
            int j = 0;
            double check_temp[MAX_CHECK_LINK] = {0};
            for (j = 0; j < MAX_CHECK_LINK; j++) {
                check_temp[j] = CHECK_INFO[laynum][j];
            }
            for (j = 0; j < check[laynum].index; j++) {
                //check_node renew in this section
                CHECK_INFO[laynum][j] = 2 * artanh(prod_info(j, laynum, &check[laynum], variable, VAR_INFO));
            }
            //variable renew in this layer
            for (j = 0; j < check[laynum].index; j++) {
                int temp = check[laynum].link_index[j];
                TOTAL_INFO[temp] = TOTAL_INFO[temp] + CHECK_INFO[laynum][j] - check_temp[j];
                VAR_INFO[temp][var_find_check(laynum, &variable[temp])] = TOTAL_INFO[temp] - CHECK_INFO[laynum][j];
            }
            
        }
#ifdef PRINT_CHAN_INFO
        
        for (i = 0; i < 1; i++) {
            for (j = 0; j < 10; j++) {
                printf("%f ", TOTAL_INFO[i * 10 + j]);
            } 
            printf("\n");
        } printf("\n");
        double temp = 0;
        for (i = 0; i < H_COLUMN_SIZE; i++) {
            temp += abs(TOTAL_INFO[i]);
        }
        mean_chaninfo[iter] = temp / H_COLUMN_SIZE;

#endif

        soft_decision(TOTAL_INFO, code_dec, VAR_NUM);

        if (stop_con(H, code_dec, CHECK_NUM))
        {
            //printf("Successfully decode.\n");
            *iter_num = iter;
            suc_flag = true;
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
    
}  
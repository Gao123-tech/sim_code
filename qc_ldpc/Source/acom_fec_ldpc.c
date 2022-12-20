#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <signal.h>
#include <stdbool.h>
#include <string.h>
#include "mex.h"
#include "acom_fec_ldpc.h"

static void swap(unsigned char*,unsigned char*);

/* int print_err[2];
FILE *fp_ldpclog;
char acom_fec_version[20] = "ldpc20211030_1501";
unsigned char scramblebits[80] = {0x9d, 0x38, 0xde, 0x95, 0xd2, 0xa3, 0xbf, 0xa0,
                                  0x5e, 0xd5, 0x87, 0x48, 0xcb, 0x59, 0xeb, 0x19,
                                  0xf9, 0x54, 0xcc, 0xa7, 0xd3, 0x84, 0x6c, 0x8a,
                                  0x6f, 0x75, 0x73, 0x3b, 0xb9, 0xd4, 0xe8, 0x3d,
                                  0xb8, 0x62, 0x52, 0xcd, 0x11, 0x69, 0x74, 0xc5,
                                  0x81, 0x49, 0x7d, 0xe3, 0x1b, 0xb0, 0xf2, 0x72,
                                  0xc4, 0x37, 0xf3, 0x8d, 0x4a, 0x10, 0x96, 0xfa,
                                  0xe2, 0xe4, 0x3e, 0xd5, 0x17, 0xb3, 0x9f, 0x07,
                                  0x25, 0x65, 0xe5, 0xc1, 0x5b, 0x30, 0xd6, 0xe8,
                                  0xaf, 0xd1, 0xcd, 0xca, 0x34, 0x0c, 0x91, 0x04};

unsigned short hadamard[16] = {0x0000, 0x5555, 0x3333, 0x6666, 0x0f0f, 0x5a5a, 0x3c3c, 0x6969,
                               0x00ff, 0x55aa, 0x33cc, 0x6699, 0x0ff0, 0x5aa5, 0x3cc3, 0x6996};

unsigned char H[H_ROW_SIZE][H_COLUMN_SIZE];
unsigned char control_bits[H_COLUMN_SIZE - H_ROW_SIZE];
// unsigned char info_bits[H_COLUMN_SIZE-H_ROW_SIZE];
// unsigned char check_bits[H_ROW_SIZE];
// unsigned char ldpccodes[H_COLUMN_SIZE];
unsigned char Ht[H_ROW_SIZE][H_COLUMN_SIZE];
struct variable_node variable[H_COLUMN_SIZE]; 
struct check_node check[H_ROW_SIZE];   */  


static void swaparr(unsigned char *arr1, unsigned char *arr2, int size)
{
    int i = 0;
    for (i = 0; i < size; i++)
    {
        swap(&arr1[i], &arr2[i]);
    }
}

static void swap(unsigned char *a, unsigned char *b)
{
    int temp;
    temp = *a;
    *a = *b;
    *b = temp;
}

static void Xor(unsigned char *arr1, unsigned char *arr2, int size)
{
    int i = 0;
    for (i = 0; i < size; i++)
    {
        arr1[i] ^= arr2[i];
    }
}

static void copy_mat(const unsigned char *arr1, unsigned char *arr2, int size)
{
    int i = 0;
    for (i = 0; i < size; i++)
    {
        arr2[i] = arr1[i];
    }
}

static bool XOR(const unsigned char *arr1, const unsigned char *arr2, int len)
{
    bool res = 0;
    int i = 0;
    for (i = 0; i < len; i++)
    {
        res ^= (arr1[i] & arr2[i]);
    }
    return res;
}

bool stop_con(unsigned char (*H)[H_COLUMN_SIZE], const unsigned char *coded, int len)
{
    int i = 0;
    for (i = 0; i < len; i++)
    {
        if (XOR(H[i], coded, H_COLUMN_SIZE))
            return false;
        // printf("%d ",XOR(H[i],coded,H_COLUMN_SIZE));
    }
    // printf("\n");
    return true;
}

/* static void scramble(str_Packet *pkt, unsigned char *scrambler, unsigned char *infobits, int bit_num)
{
    int i, j;
    unsigned char *temp;
    temp = (unsigned char *)pkt;
    unsigned char tempbits[bit_num / 8];

    for (i = 0; i < bit_num / 8; i++)
    {
        tempbits[i] = temp[i] ^ scramblebits[i];
    }

    for (i = 0; i < bit_num / 8; i++)
    {
        for (j = 0; j < 8; j++)
        {
            infobits[8 * i + j] = (tempbits[i] >> (7 - j)) & 1;
        }
    }
} */

// void acom_enc_ldpc_8fsk(str_Macinfo *macinfo, int *sym_cnt, unsigned char *enc_out)
// {
//     // printf("test3\n");
//     extern unsigned char Ht[H_ROW_SIZE][H_COLUMN_SIZE];
//     macinfo->dtlength = 256;
//     macinfo->hdrlength = 128;
//     int hdrbytes = macinfo->hdrlength / 8;
//     int dtbytes = macinfo->dtlength / 8;
//     int packetlen = macinfo->dtlength + macinfo->hdrlength;
//     unsigned char info_bits[H_COLUMN_SIZE - H_ROW_SIZE] = {0};
//     unsigned char check_bits[H_ROW_SIZE] = {0};
//     unsigned char ldpccodes[H_COLUMN_SIZE] = {0};
//     unsigned char info_8fsk[256] = {0};
//     const int k = H_COLUMN_SIZE - H_ROW_SIZE;
//     // int p[256];
//     // unsigned char temp[256];
//     *sym_cnt = 256;
//     // printf("test4\n");
//     str_Packet *packet;
//     packet = (str_Packet *)malloc(sizeof(str_Packet));
//     packet->hdr_mac.dst_addr = hadamard[macinfo->dst];
//     packet->hdr_mac.src_addr = hadamard[macinfo->src];
//     packet->hdr_mac.via_addr = hadamard[macinfo->via];
//     packet->hdr_mac.ttl = hadamard[macinfo->ttl & 0xf];
//     memcpy(packet->payload_data + hdrbytes - 10, macinfo->data, dtbytes * sizeof(char));
//     // packet->hdr_mac.crc16 = crc16(0,packet->payload_data+hdrbytes-10,dtbytes);
//     packet->hdr_mac.crc16 = 0;
//     scramble(packet, scramblebits, info_bits, packetlen);
//     // permutation(p,*sym_cnt);
//     // printf("test5\n");
//     //---------------------------------encode------------------------
//     int i, j;
//     for (i = 0; i < H_ROW_SIZE; i++)
//     {
//         unsigned char temp = 0;

//         for (j = 0; j < k; j++)
//         {
//             if (Ht[i][j] == 1)
//             {
//                 temp ^= info_bits[j];
//             }
//         }
//         int l = 0;
//         for (l = 0; l < i; l++)
//         {
//             // check_bits[i]=(check_bits[j]&H[i][j+k])^check_bits[j-1];
//             if (Ht[i][l + k] == 1)
//                 temp ^= check_bits[l];
//         }
//         if (Ht[i][i + k] == 1)
//             check_bits[i] = temp;
//         else
//         {
//             printf("The H_matrix is not inversible at rang %d!!!\n", i);
//             return;
//         }
//     }
//     // int k=H_COLUMN_SIZE-H_ROW_SIZE;
//     for (i = 0; i < H_COLUMN_SIZE; i++)
//     {
//         if (i < k)
//         {
//             ldpccodes[i] = info_bits[i];
//             // printf("%hd ",ldpccodes[i]);
//         }
//         else
//         {
//             ldpccodes[i] = check_bits[i - k];
//             // printf("%hd ",ldpccodes[i]);
//         }
//     } // printf("test6\n");
//     // printf("\n");
//     /* if(stop_con(H,ldpccodes,H_ROW_SIZE))
//     printf("success in encoding...\n");  */
//     //--------------------------------------------------------------
//     for (i = 0; i < *sym_cnt; i++)
//     {
//         info_8fsk[i] = 4 * ldpccodes[3 * i] + 2 * ldpccodes[3 * i + 1] + ldpccodes[3 * i + 2];
//         // printf("%hd ",info_8fsk[i]);
//     }
//     // printf("\n");

//     /* for(int i = 0;i < *sym_cnt;i++){
//         temp[i] = info_8fsk[i];
//     }

//     for(int i = 0;i < *sym_cnt;i++){
//         info_8fsk[i] = temp[p[i]];
//         //printf("%hd ",info_8fsk[i]);
//         for(int j=0;j<3;j++){
//             printf("%d ",(info_8fsk[i]>>(2-j))&1);
//         }
//     }printf("\n"); */
//     // printf("test7\n");
//     for (i = 0; i < *sym_cnt / 2; i++)
//     {
//         enc_out[i] = (info_8fsk[2 * i] & 0xf) + (info_8fsk[2 * i + 1] << 4);
//     }

//     free(packet);
//     // printf("test8\n");
// }

// void acom_dec_ldpc_8fsk(int (*soft_in)[8], const int sym_cnt, str_Macinfo *macinfo)
// {
//     if (sym_cnt != H_COLUMN_SIZE / 3)
//     {
//         printf("The sym_cnt is not supported in ldpc decoding!!!\n");
//         return;
//     }
//     if (fp_ldpclog == NULL)
//         printf("can not open ldpc log...\n");
//     printf("\n");
//     macinfo->dtlength = 256;
//     macinfo->hdrlength = 128;
//     const int bit_cnt = macinfo->dtlength + macinfo->hdrlength;

//     double VAR_INFO[VAR_NUM][MAX_VAR_LINK] = {{0}};
//     double CHECK_INFO[CHECK_NUM][MAX_CHECK_LINK] = {{0}};
//     double TOTAL_INFO[VAR_NUM] = {0};
//     double CHANNEL_INFO[VAR_NUM] = {0};
//     unsigned short crc_calc = 0;
//     unsigned short crc_receive = 0;
//     unsigned char coded_dec[H_COLUMN_SIZE];
//     unsigned char oringinalbits[bit_cnt]; //���Żָ�ԭʼ����
//     unsigned char oringinalbytes[bit_cnt / 8];
//     unsigned char infobit_dec[H_COLUMN_SIZE - H_ROW_SIZE]; //����ָ�����Ϣ����
//     unsigned char info_bit_check[H_COLUMN_SIZE - H_ROW_SIZE];
//     int hdrbytes = macinfo->hdrlength / 8;
//     int dtbytes = macinfo->dtlength / 8;
//     int iter = 0;
//     // int tempy[sym_cnt][8];
//     // int p[sym_cnt];
//     // double demapinfo[sym_cnt][3];

//     /* permutation(p,sym_cnt);

//     for(int i = 0;i < sym_cnt;i++){
//         for(int j = 0;j < 8;j++){
//             tempy[p[i]][j] = soft_in[i][j];
//             //printf("%d ",soft_in[i][j]);
//         }//printf("\n");
//     }printf("\n"); */
//     //-------------------------------decode---------------------

//     in_map_log_8fsk(soft_in, CHANNEL_INFO, sym_cnt);
//     //--------------------------DEBUG------------------------
//     /*  for(int i=0;i<sym_cnt;i++){
//         for(int j=0;j<8;j++){
//             printf("%d ",soft_in[i][j]);
//         }
//         printf("\n");
//     }
//     printf("\n");
//     printf("channel info:\n");
//     for(int i=0;i<384;i++){
//         printf("%f ",CHANNEL_INFO[i]);
//     }
//     printf("\n"); */
//     /*for(int i=0;i<20;i++){
//         printf("%d ",variable[i].index);
//     }
//     printf("\n"); */
//     //-------------------------------------------------------
//     initial(VAR_INFO, CHANNEL_INFO);

//     while (iter <= MAX_ITER)
//     {

//         cal_total_info(TOTAL_INFO, CHECK_INFO, CHANNEL_INFO, VAR_NUM);
//         //----------------------------------debug_print-----------------------------
//         /* printf("total info:\n");
//         for(int i=0;i<100;i++){
//             printf("%f ",TOTAL_INFO[i]);
//         }
//         printf("\n"); */
//         /* printf("channel info:\n");
//         for(int i=0;i<100;i++){
//             printf("%f ",CHANNEL_INFO[i]);
//         }
//         printf("\n");  */
//         /* for(int i=0;i<10;i++){
//             for(int j=0;j<MAX_CHECK_LINK;j++){
//                 printf("%lf ",CHECK_INFO[i][j]);
//             }printf("\n");
//         }printf("\n"); */

//         soft_decision(TOTAL_INFO, coded_dec, VAR_NUM);
//         if ((iter == 0) && (print_err[0] == 0) && (print_err[1] == 0))
//         {
//             int i;
//             // int err_cnt=0;
//             for (i = 0; i < H_COLUMN_SIZE - H_ROW_SIZE; i++)
//             {
//                 info_bit_check[i] = coded_dec[i];
//                 // if(coded_dec[i]!=control_bits[i])
//                 // err_cnt++;
//             }
//             // printf("ini_err_bits_cnt: %d\n",err_cnt);
//         }
//         //----------------------------------------debug--------------------------------------------
//         /* for(int i=0;i<100;i++){
//             printf("%hd ",coded_dec[i]);
//         } */
//         // printf("\n");
//         if (stop_con(H, coded_dec, CHECK_NUM))
//         {
//             // printf("success in decoding!!!\n");
//             int i;
//             int err_cnt = 0;
//             for (i = 0; i < H_COLUMN_SIZE - H_ROW_SIZE; i++)
//             {
//                 err_cnt += (coded_dec[i] != info_bit_check[i]);
//             }
//             printf("ini_err_bits_cnt: %d  corr_rate: 1.0\n", err_cnt);
//             fprintf(fp_ldpclog, "ini_err_bits_cnt: %d  corr_rate: 1.0\n", err_cnt);
//             macinfo->op_code = 1;
//             break;
//         }
//         // if(iter%2!=0)
//         // printf("�ڵ�%d�ε����󣬴�����ظ���Ϊ%d��\n",iter,cal_err_bits(ldpccodes,coded_dec,H_COLUMN_SIZE));
//         check_renew(VAR_INFO, CHECK_INFO, check, variable, CHECK_NUM);

//         var_renew(VAR_INFO, CHECK_INFO, TOTAL_INFO, VAR_NUM);

//         iter++;
//     }
//     if (iter == 52)
//     {
//         macinfo->op_code = 0;
//         if ((print_err[0] == 0) && (print_err[1] == 0))
//         {
//             int i;
//             int err_cnt = 0;
//             int ini_err_cnt = 0;
//             for (i = 0; i < H_COLUMN_SIZE - H_ROW_SIZE; i++)
//             {
//                 err_cnt += (coded_dec[i] != control_bits[i]);
//                 ini_err_cnt += (info_bit_check[i] != control_bits[i]);
//             }
//             printf("ini_err_bits_cnt: %d, final_err_bits_cnt: %d\n, corr_rate: %f", ini_err_cnt, err_cnt, (float)((ini_err_cnt - err_cnt) / ini_err_cnt));
//             fprintf(fp_ldpclog, "ini_err_bits_cnt: %d, final_err_bits_cnt: %d\n, corr_rate: %f", ini_err_cnt, err_cnt, (float)((ini_err_cnt - err_cnt) / ini_err_cnt));
//         }
//     }

//     //----------------------------------------------------------------
//     int i, j;
//     for (i = 0; i < bit_cnt; i++)
//     {
//         infobit_dec[i] = coded_dec[i];
//         // printf("%hd ",infobit_dec[i]);
//     }
//     // printf("\n");
//     printf("iter_num:%d\n", iter);
//     fprintf(fp_ldpclog, "iter_num: %d\n", iter);
//     descramble(infobit_dec, scramblebits, oringinalbits, bit_cnt);

//     for (i = 0; i < bit_cnt / 8; i++)
//     {
//         oringinalbytes[i] = 0;
//     }
//     for (i = 0; i < bit_cnt / 8; i++)
//     {
//         for (j = 7; j >= 0; j--)
//         {
//             oringinalbytes[i] |= oringinalbits[8 * i + 7 - j] << j;
//         }
//     } // printf("test1\n");

//     str_Packet *pkt = (str_Packet *)oringinalbytes;
//     macinfo->dst = maphadamard(pkt->hdr_mac.dst_addr, hadamard);
//     macinfo->src = maphadamard(pkt->hdr_mac.src_addr, hadamard);
//     macinfo->via = maphadamard(pkt->hdr_mac.via_addr, hadamard);
//     // crc_calc = crc16(0,pkt->payload_data+hdrbytes-10,dtbytes*sizeof(char));
//     // crc_receive = pkt->hdr_mac.crc16;
//     /*if( crc_calc != crc_receive){
//         macinfo->op_code = 0;
//     }
//     else{
//         macinfo->op_code = 1;
//     }*/

//     macinfo->ttl = maphadamard(pkt->hdr_mac.ttl, hadamard);
//     for (i = 0; i < macinfo->dtlength; i++)
//     {
//         if (*(pkt->payload_data + hdrbytes - 10 + i) > 0 && *(pkt->payload_data + hdrbytes - 10 + i) < 32 || *(pkt->payload_data + hdrbytes - 10 + i) > 126)
//         {
//             *(pkt->payload_data + hdrbytes - 10 + i) = 32;
//         }
//     }
//     memcpy(macinfo->data, pkt->payload_data + hdrbytes - 10, dtbytes * sizeof(char));
//     macinfo->data[dtbytes] = 0;
//     // printf("test2\n");
// }

void ini_matrix(mattype H, struct variable_node *variable, struct check_node *check)
{
    // extern struct variable_node variable[];
    // extern struct check_node check[];
    //produce_qc_H_Ht();
    int i, j;
    for (i = 0; i < H_ROW_SIZE; i++)
    {
        for (j = 0; j < H_COLUMN_SIZE; j++)
        {
            if (H[i][j] == 1)
            {
                variable[j].link_index[variable[j].index] = i;
                check[i].link_index[check[i].index] = j;
                variable[j].index++;
                check[i].index++;
            }
        }
    }
}

void initial(double (*H)[MAX_VAR_LINK], const double channel_info[], const struct variable_node *variable)
{
    //extern struct variable_node variable[H_COLUMN_SIZE];
    int i, j;
    for (i = 0; i < VAR_NUM; i++)
    {
        for (j = 0; j < variable[i].index; j++)
        {
            H[i][j] = channel_info[i]; 
        }
    }
}

void check_renew(const double (*var_info)[MAX_VAR_LINK], double (*check_info)[MAX_CHECK_LINK], const struct check_node *check, const struct variable_node *variable, int num)
{
    int i, j;
    for (i = 0; i < num; i++)
    {
        for (j = 0; j < check[i].index; j++)
        {
            check_info[i][j] = 2 * artanh(prod_info(j, i, &check[i], variable, var_info));
        }
    }
}

void var_renew(double (*var_info)[MAX_VAR_LINK], double (*check_info)[MAX_CHECK_LINK], const double *total_info, const struct variable_node *variable, const struct check_node *check, int num)
{
    int i, j;
    for (i = 0; i < num; i++)
    {
        for (j = 0; j < variable[i].index; j++)
        {
            int temp = check_find_var(i, &check[variable[i].link_index[j]]);
            var_info[i][j] = total_info[i] - check_info[variable[i].link_index[j]][temp];
        }
    }
}

void cal_total_info(double *total_info, const double (*check_info)[MAX_CHECK_LINK], const double *channel_info, const struct variable_node *variable, const struct check_node *check, int num)
{
    double res = 0;
    int i, j;
    for (i = 0; i < num; i++)
    {
        res = 0;
        for (j = 0; j < variable[i].index; j++)
        {
            int temp = check_find_var(i, &check[variable[i].link_index[j]]);
            res += check_info[variable[i].link_index[j]][temp]; //��������ڵ�������У��ڵ���Ϣ�����ۼӣ���������ڵ������Ϣ
        }
        total_info[i] = res + channel_info[i];
    }
}

int check_find_var(int var_index, const struct check_node *check_node)
{
    int i = 0;
    for (i = 0; i < check_node->index; i++)
    {
        if (check_node->link_index[i] == var_index)
            return i;
    }
    printf("warning in function check_find_var!!!\n");
    return -1;
}

int var_find_check(int check_index, const struct variable_node *variable_node)
{
    int i = 0;
    for (i = 0; i < variable_node->index; i++)
    {
        if (variable_node->link_index[i] == check_index)
            return i;
    }
    printf("warning in function var_find_check!!!\n");
    return -1;
    // exit(EXIT_FAILURE);
}

double prod_info(int ex_var_index, int check_index, const struct check_node *check_node, const struct variable_node *variable, const double (*var_info)[MAX_VAR_LINK])
{
    double res = 1.0;
    int i = 0;
    for (i = 0; i < check_node->index; i++)
    {
        if (i == ex_var_index)
            continue;
        int temp = var_find_check(check_index, &variable[check_node->link_index[i]]);
        res *= tanh(0.5 * var_info[check_node->link_index[i]][temp]);
    }
    if (res == 1)
    {
        // printf("Something wrong in function prod_info!!!\n");
        // exit(EXIT_FAILURE);
        res -= 0.000001;
    }
    else if (res == -1)
    {
        res += 0.000001;
    }
    else if (res > 1 || res < -1)
    {
        printf("something wrong in function prod_info...\n");
        // exit(EXIT_FAILURE);
        return -1;
    }
    // printf("%lf\n",res);
    return res;
}

double artanh(double x)
{
    if ((x <= -1) || (x >= 1))
    {
        printf("Error input in function artanh!!!\n");
        exit(EXIT_FAILURE);
    }
    return 0.5 * log((1 + x) / (1 - x));
}

void soft_decision(const double *total_info, unsigned char *coded_dec, int num)
{
    int i = 0;
    for (i = 0; i < num; i++)
    {
        coded_dec[i] = (total_info[i] >= 0) ? 0 : 1;
        //coded_dec[i] = (total_info[i] >= 0) ? 1 : 0;
    }
}

static void read_matrix(unsigned char (*arr)[H_COLUMN_SIZE], const char *file_name, int size)
{
    FILE *fp = NULL;
    if (!(fp = fopen(file_name, "r")))
    {
        printf("fail to open file %s\n", file_name);
        return;
    }
    short int temp;
    int i, j;
    for (i = 0; i < size; i++)
    {
        for (j = 0; j < H_COLUMN_SIZE; j++)
        {
            fscanf(fp, "%hd", &temp);
            arr[i][j] = temp;
        }
    }
    fclose(fp);
}

/* static void in_map_log_8fsk(const int (*soft_in)[8], double *channel_info, int size)
{
    int a, b, c, d, e, f;
    int i = 0;
    for (i = 0; i < size; i++)
    {
        a = soft_in[i][0] + soft_in[i][1] + soft_in[i][2] + soft_in[i][3];
        b = soft_in[i][4] + soft_in[i][5] + soft_in[i][6] + soft_in[i][7];
        c = soft_in[i][0] + soft_in[i][1] + soft_in[i][4] + soft_in[i][5];
        d = soft_in[i][2] + soft_in[i][3] + soft_in[i][6] + soft_in[i][7];
        e = soft_in[i][0] + soft_in[i][2] + soft_in[i][4] + soft_in[i][6];
        f = soft_in[i][1] + soft_in[i][3] + soft_in[i][5] + soft_in[i][7];
        if (a == 0 && b == 0)
        {
            channel_info[3 * i] = 0;
        }
        else
            channel_info[3 * i] = 2 * (a - b) / sqrt(b * b + a * a);
        if (c == 0 && d == 0)
        {
            channel_info[3 * i + 1] = 0;
        }
        else
            channel_info[3 * i + 1] = 2 * (c - d) / sqrt(d * d + c * c);
        if (f == 0 && e == 0)
        {
            channel_info[3 * i + 2] = 0;
        }
        else
            channel_info[3 * i + 2] = 2 * (e - f) / sqrt(f * f + e * e);
    }
}

static void descramble(unsigned char *infobits, unsigned char *scrambler, unsigned char *originalbits, int bit_num)
{
    int i, j;
    unsigned char info[bit_num / 8];
    unsigned char descramble_info[bit_num / 8];
    for (i = 0; i < bit_num / 8; i++)
    {
        info[i] = 0;
        descramble_info[i] = 0;
    }

    for (i = 0; i < bit_num / 8; i++)
    {
        for (j = 7; j >= 0; j--)
        {
            info[i] |= infobits[8 * i + 7 - j] << j;
        }
    }
    for (i = 0; i < bit_num / 8; i++)
    {
        descramble_info[i] = info[i] ^ scramblebits[i];
    }
    for (i = 0; i < bit_num / 8; i++)
    {
        for (j = 0; j < 8; j++)
        {
            originalbits[8 * i + j] = (descramble_info[i] >> (7 - j)) & 1;
        }
    }
}

static int maphadamard(unsigned short temp, unsigned short *hadamard)
{
    int distance = 16;
    int index = 0;
    int i;
    for (i = 0; i < 16; i++)
    {
        if (distance > calc_distance(temp, hadamard[i]))
        {
            distance = calc_distance(temp, hadamard[i]);
            index = i;
        }
    }
    return index;
}

static int calc_distance(unsigned short a, unsigned short b)
{
    unsigned short tmp;
    int count = 0;
    tmp = a ^ b;
    while (tmp)
    {
        tmp = tmp & (tmp - 1);
        count++;
    }
    return count;
}

void permutation(int *a, int length)
{
    int f1, f2, i;
    if (length == 256)
    {
        f1 = 15;
        f2 = 32;
    }
    else if (length == 384)
    {
        f1 = 23;
        f2 = 48;
    }
    else if (length == 640)
    {
        f1 = 39;
        f2 = 80;
    }
    else if (length == 320)
    {
        f1 = 21;
        f2 = 120;
    }
    else
    {
        printf("Can't permutate!!The packet length %d is not supported.", length);
    }
    for (i = 0; i < length; i++)
    {
        a[i] = f1 * i + f2 * i * i;
        a[i] = a[i] % length;
    }
} */

void produce_qc_H_Ht(unsigned char (*H)[H_COLUMN_SIZE], unsigned char (*Ht)[H_COLUMN_SIZE])
{
    int Hb_row_size = 12;
    int Hb_column_size = 24;
    int m = H_ROW_SIZE;
    int n = H_COLUMN_SIZE;
    int Hb[12][24] = {{-1, 94, 73, -1, -1, -1, -1, -1, 55, 83, -1, -1, 7, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                      {-1, 27, -1, -1, -1, 22, 79, 9, -1, -1, -1, 12, -1, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1},
                      {-1, -1, -1, 24, 22, 81, -1, 33, -1, -1, -1, 0, -1, -1, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1},
                      {61, -1, 47, -1, -1, -1, -1, -1, 65, 25, -1, -1, -1, -1, -1, 0, 0, -1, -1, -1, -1, -1, -1, -1},
                      {-1, -1, 39, -1, -1, -1, 84, -1, -1, 41, 72, -1, -1, -1, -1, -1, 0, 0, -1, -1, -1, -1, -1, -1},
                      {-1, -1, -1, -1, 46, 40, -1, 82, -1, -1, -1, 79, 0, -1, -1, -1, -1, 0, 0, -1, -1, -1, -1, -1},
                      {-1, -1, 95, 53, -1, -1, -1, -1, -1, 14, 18, -1, -1, -1, -1, -1, -1, -1, 0, 0, -1, -1, -1, -1},
                      {-1, 11, 73, -1, -1, -1, 2, -1, -1, 47, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, -1, -1, -1},
                      {12, -1, -1, -1, 83, 24, -1, 43, -1, -1, -1, 51, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, -1, -1},
                      {-1, -1, -1, -1, -1, 94, -1, 59, -1, -1, 70, 72, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, -1},
                      {-1, -1, 7, 65, -1, -1, -1, -1, 39, 49, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0},
                      {43, -1, -1, -1, -1, 66, -1, 41, -1, -1, -1, 26, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0}};
    // extern unsigned char H[H_ROW_SIZE][H_COLUMN_SIZE];
    // extern unsigned char Ht[H_ROW_SIZE][H_COLUMN_SIZE];
    if (H_COLUMN_SIZE % Hb_column_size != 0 || H_ROW_SIZE % Hb_row_size != 0)
    {
        printf("Something wrong in function produce_qc_H!!!\n");
        return;
    }
    int squ_size = H_ROW_SIZE / Hb_row_size;
    // int suq_column_size=H_COLUMN_SIZE/Hb_column_size;
    int i, j, k;
    for (i = 0; i < Hb_row_size; i++)
    {
        for (j = 0; j < Hb_column_size; j++)
        {
            if (Hb[i][j] < 0)
            {
                /* for(int k=0;k<squ_size;k++){
                    for(int l=0;l<squ_size;l++){
                        H[i*Hb_row_size+k][j*Hb_column_size+l]=0;
                    }
                } */
                continue;
            }
            else if (Hb[i][j] == 0)
            {
                for (k = 0; k < squ_size; k++)
                {
                    H[i * squ_size + k][j * squ_size + k] = 1;
                    Ht[i * squ_size + k][j * squ_size + k] = 1;
                }
            }
            else
            {
                int trans_para = Hb[i][j] * squ_size / 96;
                for (k = 0; k < squ_size; k++)
                {
                    H[i * squ_size + (trans_para % squ_size + k) % squ_size][j * squ_size + k] = 1;
                    Ht[i * squ_size + (trans_para % squ_size + k) % squ_size][j * squ_size + k] = 1;
                }
            }
        }
    }
    bool inv = 1;
    for (i = m - 2, j = 1; i >= 0; i--, j++)
    {
        if (Ht[i + 1][n - j] == 0)
        {
            int index = i;
            // bool inv=1;
            while (index >= 0)
            {
                if (Ht[index][n - j] != 0)
                {
                    swaparr(Ht[i + 1], Ht[index], n);
                    break;
                }
                index--;
            }
            if (index == -1)
            {
                if (inv)
                {
                    inv = 0;
                    printf("The H_matrix is not inversible!!!\n");
                    continue;
                }
                else
                    continue;
            }
        }
        int N;
        for (N = i; N >= 0; N--)
        {
            if (Ht[N][n - j] == 0)
                continue;
            Xor(Ht[N], Ht[i + 1], n - j + 1);
        }
    }
}

/* int main(int argc,char *argv[]){
    ini_matrix();
    unsigned char enc_out[256];
    int sym_cnt=256;
    str_Macinfo mac={
        10,
        5,
        0,
        2,
        0,
        "Bonjour! Comment tu t'appelles?",
        128,
        256
    };
    acom_enc_ldpc_8fsk(&mac,&sym_cnt,enc_out);
    FILE *fp;
    if((fp=fopen("ldpc_8fsk_enc_out.txt","w"))){
        for(int i=0;i<sym_cnt/2;i++){
            fprintf(fp,"%02x\n",enc_out[i]);
        }
    }
    fclose(fp);
}  */
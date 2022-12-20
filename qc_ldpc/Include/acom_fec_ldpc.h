#ifndef _ACOM_FEC_LDPC_H_
#define _ACOM_FEC_LDPC_H_
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <signal.h>
#include <stdbool.h>
#include <string.h>
#define H_COLUMN_SIZE 2400
#define H_ROW_SIZE 1200
#define BASE_MAT_SIZE 10
#define Hb_ROW_SIZE 12
#define Hb_COLUMN_SIZE 24
#define MAX_ITER 50
#define MAX_VAR_LINK 6
#define MAX_CHECK_LINK 7
#define VAR_NUM 2400
#define CHECK_NUM 1200

typedef unsigned char (*mattype)[H_COLUMN_SIZE];

/* typedef struct mac_hdr
{
	unsigned short dst_addr;
	unsigned short src_addr;
	unsigned short via_addr;
	unsigned short crc16;
	unsigned short ttl;
}str_Header;

typedef struct mac_packet
{
	str_Header hdr_mac;
	unsigned char payload_data[70];
}str_Packet;

typedef struct mac_info
{
	unsigned char dst;
    unsigned char src;
	unsigned char via;
	unsigned char ttl;
	unsigned char op_code;
	unsigned char data[65];
	int hdrlength;
	int dtlength;
}str_Macinfo; */

struct variable_node
{
    //unsigned int link_num;
    int index;
    int link_index[MAX_VAR_LINK];
};

struct check_node
{
    //unsigned int link_num;
    int index;
    int link_index[MAX_CHECK_LINK];
};

static void swaparr(unsigned char*,unsigned char *,int size);

static void Xor(unsigned char*,unsigned char *,int size);

static void copy_mat(const unsigned char *,unsigned char *,int);

static unsigned char (*produce_squa_mat(void))[BASE_MAT_SIZE];

static bool XOR(const unsigned char*,const unsigned char *,int);

bool stop_con(unsigned char (*H)[H_COLUMN_SIZE],const unsigned char *coded,int len);

//void produce_qc_H(int Hb_row_size,int Hb_column_size);

//void acom_enc_ldpc_8fsk(str_Macinfo *macinfo,int *sym_cnt,unsigned char *enc_out);
//------------------------------------------------------
//void acom_dec_ldpc_8fsk(int (*soft_in)[8],const int sym_cnt,str_Macinfo *macinfo);

void initial(double(*)[MAX_VAR_LINK],const double channel_info[], const struct variable_node *);

void check_renew(const double (*var_info)[MAX_VAR_LINK],double (*check_info)[MAX_CHECK_LINK],const struct check_node* check,const struct variable_node *variable, int num);

void var_renew(double (*var_info)[MAX_VAR_LINK],double (*check_info)[MAX_CHECK_LINK],const double *total_info, const struct variable_node *, const struct check_node *, int num);

void cal_total_info(double *,const double (*)[MAX_CHECK_LINK],const double *, const struct variable_node *, const struct check_node *, int num);

int check_find_var(int var_index,const struct check_node*); //�������ض������ڵ����������кţ���У�������Ϣ������

int var_find_check(int check_index,const struct variable_node*);

double prod_info(int,int,const struct check_node*,const struct variable_node*,const double(*)[MAX_VAR_LINK]);

double artanh(double x);

void soft_decision(const double*,unsigned char *,int num);

static void read_matrix(unsigned char (*)[H_COLUMN_SIZE],const char *,int size);

//static void in_map_log_8fsk(const int (*dm_info)[8],double *channel_info,int size);

void ini_matrix(mattype, struct variable_node*, struct check_node*);

//static void descramble(unsigned char *infobits,unsigned char *scrambler,unsigned char *originalbits,int bit_num);

//static int maphadamard(unsigned short temp,unsigned short *hadamard);

//static int calc_distance(unsigned short a,unsigned short b);

//void permutation(int* a,int length);

void produce_qc_H_Ht(unsigned char (*)[H_COLUMN_SIZE], unsigned char (*)[H_COLUMN_SIZE]);

#endif
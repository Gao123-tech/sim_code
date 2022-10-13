#ifndef __LDPC_TOOL__
#define __LDPC_TOOL__
#define BAR_TO_STOP 1.0
#include "acom_fec_ldpc.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void searchGirth(const unsigned char (*H)[H_COLUMN_SIZE], int len); //search for the gitrh of the tanner graph

void getDegree(const unsigned char (*H)[H_COLUMN_SIZE], int len);

bool stop_ahead(const double*, const double*, int); //calculate the difference of the soft information between the pre and the cur iteration

void quicksort(double*, int l, int r);

int partition(double *, int left, int right);

void test_channel(const double *, int); //evaluate the channel state to choose the code rate in the communication

inline void cal_diff_llr(double *cur_diff_llr, double *pre_llr, double *cur_llr, int len);

#endif
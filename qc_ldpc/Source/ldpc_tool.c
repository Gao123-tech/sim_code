#include "ldpc_tool.h"
#include <time.h>

typedef struct puncRec
{
    int *puncSet;
    int num;
}puncRec;


static void swap(double *, double *);

bool stop_ahead(const double *ini_channel_info, const double *cur_channel_info, int len)
{
    int i;
    double diff = 0.0;
    for (i = 0; i < len; i++)
    {
        double curdiff = abs(cur_channel_info[i] - ini_channel_info[i]);
        diff = curdiff > diff ? curdiff : diff;
    }
    if (diff > BAR_TO_STOP)
    {
        return true;
    }
    else
    {
        return false;
    }
}

//randomly choose a member as the pivot and make partition
int partition(double *arr, int start, int end) {
    srand(time(NULL));
    int pivotIndex = rand() % (end - start + 1) + start;
    double pivot = arr[pivotIndex];
    swap(arr + pivotIndex, arr + end);
    int index = start;
    int i = 0;
    for (i = start; i < end; i++) {
        if (arr[i] < pivot) {
            swap(arr + i, arr + index);
            index++;
        }
    }
    swap(arr + index, arr + end);
}

static void swap(double *x, double *y) {
    double temp = 0.0;
    temp = *x;
    *x = *y;
    *y = temp;
}

void quicksort(double *arr, int left, int right) {
    if (left >= right) {
        return;
    }
    int k = partition(arr, left, right);
    quicksort(arr, left, k - 1);
    quicksort(arr, k + 1, right);
}

//find the kth number of two arrays
static double findkth(const double *arr1, int left1, int right1, const double *arr2, int left2, int right2, int k) {
    if (left1 > right1) {
        return arr2[left2 + k - 1];
    }
    if (left2 > right2) {
        return arr1[left1 + k -1];
    }
    if (k == 1) {
        return arr1[left1] < arr2[left2] ? arr1[left1] : arr2[left2];
    }
    int mid1 = __INT_MAX__;
    int mid2 = __INT_MAX__;
    if (left1 + k / 2 - 1 <= right1) {
        mid1 = arr1[left1 + k / 2 -1];
    }
    if (left2 + k / 2 - 1 <= right2) {
        mid2 = arr2[left2 + k / 2 - 1];
    }
    if (mid1 < mid2) {
        return findkth(arr1, left1 + k / 2, right1, arr2, left2, right2, k - k / 2);
    }
    return findkth(arr1, left1, right1, arr2, left2 + k / 2, right2, k - k / 2);
}

double findmid(const double *arr1, int len1, const double *arr2, int len2) {
    if (len1 <= 0) {
        return arr2[len2 / 2];
    }
    if (len2 <= 0) {
        return arr1[len1 / 2];
    }
    int len = len1 + len2;
    int k1 = (len + 1) / 2, k2 = (len + 2) / 2;
    if (len1 == 0) {
        return (arr2[(len2 + 1) / 2 - 1] + arr2[(len2 + 2) / 2 - 1]) / 2;
    } else if (len2 == 0) {
        return (arr1[(len1 + 1) / 2 - 1] + arr1[(len1 + 2) / 2 - 1]) / 2;
    } else {
        double mid1 = findkth(arr1, 0, len1 - 1, arr2, 0, len2 - 1, k1);
        double mid2 = findkth(arr1, 0, len1 - 1, arr2, 0, len2 - 1, k2);
        return (mid1 + mid2) / 2;
    }
}

inline void cal_diff_llr(double *cur_diff_llr, double *pre_llr, double *cur_llr, int len) {
    int i = 0;
    for (i = 0; i < len; i++) {
        cur_diff_llr = abs(cur_llr[i] - pre_llr[i]);
    }
}

        //    [-1 94 73 -1 -1 -1 -1 -1 55 83 -1 -1 7 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
        //     -1 27 -1 -1 -1 22 79 9 -1 -1 -1 12 -1 0 0 -1 -1 -1 -1 -1 -1 -1 -1 -1
        //     -1 -1 -1 24 22 81 -1 33 -1 -1 -1 0 -1 -1 0 0 -1 -1 -1 -1 -1 -1 -1 -1
        //     61 -1 47 -1 -1 -1 -1 -1 65 25 -1 -1 -1 -1 -1 0 0 -1 -1 -1 -1 -1 -1 -1
        //     -1 -1 39 -1 -1 -1 84 -1 -1 41 72 -1 -1 -1 -1 -1 0 0 -1 -1 -1 -1 -1 -1
        //     -1 -1 -1 -1 46 40 -1 82 -1 -1 -1 79 0 -1 -1 -1 -1 0 0 -1 -1 -1 -1 -1
        //     -1 -1 95 53 -1 -1 -1 -1 -1 14 18 -1 -1 -1 -1 -1 -1 -1 0 0 -1 -1 -1 -1
        //     -1 11 73 -1 -1 -1 2 -1 -1 47 -1 -1 -1 -1 -1 -1 -1 -1 -1 0 0 -1 -1 -1
        //     12 -1 -1 -1 83 24 -1 43 -1 -1 -1 51 -1 -1 -1 -1 -1 -1 -1 -1 0 0 -1 -1
        //     -1 -1 -1 -1 -1 94 -1 59 -1 -1 70 72 -1 -1 -1 -1 -1 -1 -1 -1 -1 0 0 -1
        //     -1 -1 7 65 -1 -1 -1 -1 39 49 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 0 0
        //     43 -1 -1 -1 -1 66 -1 41 -1 -1 -1 26 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 0]


puncRec ramdomly_punc(int *punc_set, double pre_rate, double cur_rate, int len) {
    srand(time(NULL));
    int puncNum = (int)(len * (pre_rate - cur_rate));
    int i;
    puncRec ret;
    ret.num = puncNum;
    ret.puncSet = (int *)malloc(sizeof(int) * puncNum);
    for (i = 0; i < puncNum; i++) {
        ret.puncSet[i] = rand() % len;
    }
    return ret;
}

double GaussianNoise(double mu, double sigma) {
    double U1, U2, W, mult;
    static double X1, X2;
    static int call = 0;
    if (call == 1) {
        call = 1 - call;
        return (mu + sigma * X2);
    }
    do {
        U1 = ((double)rand() / RAND_MAX) * 2 - 1;
        U2 = ((double)rand() / RAND_MAX) * 2 - 1;
        W = U1 * U1 + U2 * U2;
    } while (W >= 1 || W == 0);
    mult = sqrt((-2 * log(W)) / W);
    X1 = U1 * mult;
    X2 = U2 * mult;
    call = 1 - call;
    return (mu + sigma * X1);
}
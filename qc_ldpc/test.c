#include <stdio.h>
#include <string.h>
#include <time.h>

char s[50];

int main() {
    extern char s[];
    {
		time_t t;
		struct tm *cur_time;
		time(&t);
		cur_time=localtime(&t);
		char test_time[50];
		strftime(test_time,50,"test time: %Y-%m-%d %H:%M:%S\n", cur_time);
		sprintf(s, "test time: %s\n", test_time);
    }
    printf(s);
}

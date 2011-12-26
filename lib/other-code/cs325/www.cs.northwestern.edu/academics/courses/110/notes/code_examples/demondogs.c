/* This program creates a customer receipt for a hot dog stand. */

#include<stdio.h>

#define  HOTDOG_UNIT_PRICE 	2.35   /* in dollars */
#define  SODA_UNIT_PRICE 	1.50   /* in dollars */
#define  TAX_RATE 			0.08

int main ()   {

	int num_hotdogs, num_sodas; 	/* number of hotdogs and sodas bought */

	/* All cash amounts are in dollars */
	double hotdog_total, soda_total;	/* cost of hotdogs and sodas ordered */
	double subtotal; 					/* sale amount, before taxes */
	double total; 						/* sale amount, after taxes */
	double tax, cash_tendered, change;
		
	printf("Enter number of hot dogs: ");
	scanf(" %d", &num_hotdogs);
	printf("Enter number of sodas: ");
	scanf(" %d", &num_sodas);

	hotdog_total = HOTDOG_UNIT_PRICE * num_hotdogs;
	soda_total = SODA_UNIT_PRICE * num_sodas;
	subtotal = hotdog_total + soda_total;
	tax = subtotal * TAX_RATE;
	total = subtotal + tax;
	
	printf("Total = %.2f \nEnter cash tendered (in dollars): ", total);
	scanf("%lf", &cash_tendered);

	change = cash_tendered - total;

	printf("Printing receipt...\n");
	printf("---------------------------------------");
	printf("\n\n\n\t  *** DEMON DOGS ***\n\n");
	printf("\t944 W. Fullerton, Chicago\n\n\n");
	printf("Hot Dogs %3d x %.2f \t %.2f\n", num_hotdogs, HOTDOG_UNIT_PRICE, hotdog_total);
	printf("Sodas    %3d x %.2f_\t %.2f\n", num_sodas, SODA_UNIT_PRICE, soda_total);
	printf("Subtotal \t\t%.2f\n", subtotal);
	printf("Tax (%.2f\%)\t\t %.2f\n", TAX_RATE*100.0, tax);
	printf("\nTotal\t\t\t %.2f\n", total);
	printf("\nCash\t\t\t %.2f\n", cash_tendered);
	printf("\nChange\t\t\t %.2f\n", change);
	printf("\n\n\t*** THANK YOU! ***\n\n");
	printf("---------------------------------------\n\n");

	return 0;
}


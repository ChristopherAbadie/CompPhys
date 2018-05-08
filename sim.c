/* Nagel–Schreckenberg Traffic Simulation */
/* Christopher Abadie */
/* June 2015 - August 2016 */

/* Output Files:
	dist.out - counts the density of the cars every TStep
	time.out - counts how many cars finish a lap every TStep
*/

#include <iostream>
#include <iomanip>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fstream>
#define MAXCAR 10000

using namespace std;

//Prototypes
void InputVar(int&, int&, int&, float&, int&, int&);
void OpenFile(ofstream&, ofstream&); 
void Header(ofstream&, ofstream&, int, int, int, float, int, int, int); 
void Initial(int*, int*, int*, int*, int, int, int*, int*);
void displayres(int, int pos[], int vel[]); 
float average(int, int vel[]); 
void PrintRes(ofstream&, ofstream&, int, int, int, int, int distcount[], int timecount[]);

//Main Function
int main()
{
	// Define Input Variables
	int TrackL, VMax, TMax, NumCar, Seed;
	float Prob;

	//Get input variables
	InputVar(TrackL, VMax, TMax, Prob, NumCar, Seed);

	//Define more variables
	srand(Seed); 
	int pslow; 
	pslow=Prob*RAND_MAX; //slow down prob
	float aver; // value for average velocity
	int pos[MAXCAR]; // position array 
	int vel[MAXCAR]; // velocity array
	int dist[MAXCAR]; // distance between each car array
	int distcount[MAXCAR]; //count array 
	int time[MAXCAR]; //lap array
	int timecount[MAXCAR]; //count for lap array
	int c; //car number
	int space;  //space between consecutive cars
	int t; //time
	int d; // distance value 
	int TStep=10; //how many time steps for time count

	//Output Files
	ofstream timeout, distout;	
	OpenFile(timeout, distout);	

	//display input variables to screen / output files
	Header(distout, timeout, TrackL, VMax, TMax, Prob, NumCar, Seed, TStep);

	//Initialize Setup	
	Initial(dist, distcount, time, timecount, NumCar, TrackL, pos, vel);

	//Simulation 
	for (t=0; t<TMax; t++) //Time update
	{	
		pos[NumCar]=pos[0]; //circular track 
		for (c=0; c<NumCar; c++) //Speed update
		{
			if (vel[c]<VMax) {++vel[c];} //If not at max vel, increase by 1

			space=pos[c+1]-pos[c]-1;

			if (space<0)//If car in front gets to end of track	
			{
				space += TrackL;
			} 

			if (vel[c]>space) {vel[c]=space;} //To prevent accidents
			
			if(vel[c]>0 && rand()<pslow) {--vel[c];} //Random braking
		}

		for (c=0; c<NumCar; c++) //Position update
		{
			pos[c]+=vel[c];
			if (pos[c]>= TrackL)
			{
				pos[c]-=TrackL;
				time[(int)floor(t/(double)TStep)]++; //count towards lapping	
			}
		}

	//Analysis	
		
		//distance updates every TStep
		for(c=0; t%TStep==0 && c<NumCar; c++)
		{
			dist[c]=pos[c+1]-pos[c]-1; 
			if (dist[c]<0) {dist[c] += TrackL;} //car gets to end

			distcount[dist[c]]++; //count for histogram 
		}

		//time updates
		for(int b=0; t==(TMax-1) && b<(TMax/TStep); b++)
		{
			timecount[time[b]]++; //count for histogram 
		}

//		displayres(NumCar, pos, vel); //“animation” to screen

		//calculate average velocity
//		if(t%TStep==0) // calc avg vel every TStep 
//		{
//			aver=average(NumCar, vel);
//			cout<<aver<<"\n";
//		}

	}

	//Print to output files
	PrintRes(distout, timeout, TrackL, VMax, NumCar, TStep, distcount, timecount);
	
	//close files
	timeout.close();
	distout.close();
	cout<<endl;
}



/******************************/



//Input Variables
void InputVar(int& TrackL, int& VMax, int& TMax, float& Prob, int& NumCar, int& Seed)	
{
//	printf("Enter Track Length\n");
//	cin>>TrackL;
	TrackL=10000;
	
//	printf("Enter Max Velocity\n");
//	cin>>VMax;
	VMax=10;
	
//	printf("Enter amount of time\n");
//	cin>>TMax;
	TMax=100000;
	
//	printf("Enter slow down probability\n");
//	cin>>Prob;
	Prob=0.10;
//	
	printf("Enter number of cars\n");
	cin>>NumCar;
//	NumCar=10;
	
//	printf("Enter random number seed\n");
//	cin>>Seed;
	Seed=1;	
}



//Open Output file
void OpenFile(ofstream &timeout, ofstream &distout)
{
	timeout.open("time.out");	
	distout.open("dist.out");	
}



//Headers on Output File
void Header(ofstream &distout, ofstream &timeout, int TrackL, int VMax, 
int TMax, float Prob, int NumCar, int Seed, int TStep)	
{
	//screen
	cout<<"\n TrackL:"<<TrackL<<"\n VMax: "<<VMax;
	cout<<"\n Prob: "<<Prob<<"\n NumCar: "<<NumCar;
	cout<<"\n TMax: "<<TMax<<"\n TStep: "<<TStep<<endl;
	
	//countout output file
	distout<<TrackL<<"\n"<<VMax<<"\n"<<Prob;
	distout<<"\n"<<NumCar<<"\n"<<TMax;
	distout<<"\n"<<Seed<<"\n"<<TStep<<"\n";
	distout<<"//x-distance.between.two.cars\n";
	distout<<"//y-how.many.instances.in.total.time\n";	

	//timeout output file
	timeout<<TrackL<<"\n"<<VMax<<"\n"<<Prob;
	timeout<<"\n"<<NumCar<<"\n"<<TMax;
	timeout<<"\n"<<Seed<<"\n"<<TStep<<"\n";
	timeout<<"//x-how.many.cars.pass.in.a.given.time.step\n";
	timeout<<"//y-how.many.instances.in.total.time\n";

}



//Initialize arrays to 0 and setup for simulation
void Initial(int dist[MAXCAR], int distcount[MAXCAR], int time[MAXCAR],
int timecount[MAXCAR], int NumCar, int TrackL, int pos[MAXCAR], int vel[MAXCAR])
{
	//initialize arrays to 0
	for(int i=0; i<= MAXCAR; i++) 
	{
			dist[i]=0;
			distcount[i]=0;
			time[i]=0;
			timecount[i]=0;
			pos[i]=0;
			vel[i]=0;
	}
 	
	//cars equally spaced and not moving
	double psn;
	for (int c=0; c<NumCar; c++)
	{
		psn=((double)TrackL/(double)NumCar); /* estimate for intial distance */
		pos[c]=(int)floor(c*psn+0.5); /*Cars equally spread out*/
		vel[c]=0; /*Not moving at beginning*/
	}
}



//“Animation” on Screen
void displayres(int NumCar,int pos[],int vel[]) 
{
	char string[100];

	int i, k;

	for (i=0; i<100; i++)
	{
		string[i]='_';
	}
	string[99]='\0';

	for (i=0; i<NumCar; i++)
	{
		k=pos[i];
		if (k<99 && k>=0)
		{
			string[k]='O';
			if (i==0) {string[k]='x';}
		}
	}
	printf("%s\n",string);
}



//Calculate average speed
float average(int NumCar, int vel[]) 
{
	int i,number;
	float result;
	number=vel[0];

	for(i=1;i<NumCar;i++)
	{
		number=vel[i]+number; /*add up all velocities*/
	}

	result=number/(float)NumCar; /* average velocity */

	return result;
}



//Print results to output files
void PrintRes(ofstream& distout, ofstream& timeout, int TrackL, int VMax, 
int NumCar, int TStep, int distcount[MAXCAR], int timecount[MAXCAR])
{
	//print distcount array to file
	for(int d=0; d<(TrackL-NumCar); d++)
	{
		if(distcount[d]!=0)
		{
			distout<<d<<" ";
			distout<<distcount[d]<<"\n";
		}
	}

	//print timecount array to file
	for(int a=0; a<=(VMax/(double)TrackL*NumCar*TStep)*2; a++)
	{
		if(timecount[a]!=0)
		{
			timeout<<a<<" ";
			timeout<<timecount[a]<<"\n";
		}
	}

}

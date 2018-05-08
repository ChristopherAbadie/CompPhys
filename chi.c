/*Chi Squared Analysis*/
/* Christopher Abadie */
/* April 2016 - August 2016 */

/*Input File:
	time.out

  Output File:
	chi.out - chi square analysis of time.out data
*/

#include <iostream>
#include <iomanip>
#include <math.h>
#include <fstream>
#define MAX 10000
#define E 2.7182818284590452353602874
using namespace std;

//prototypes
void OpenFile(ifstream&, ofstream&); 
void Chi(ofstream&, int*, int, double&, double&, int, int);
double Poisson(double, int);
void PrintRes(ofstream&, int, double, double, int, int, int, double, double);

//main function
int main()
{
	//open input & output files
	ifstream fin;
	ofstream fout;
	OpenFile(fin, fout);

	//define input variables
	int TrackL;
	double VMax;
	double Prob;
	int NumCar;
	int TMax;
	int Seed;
	int TStep;
	string line1;
	string line2;
	int y[MAX];
		
	//read input file header
	fin>>TrackL;
	fin>>VMax;
	fin>>Prob;
	fin>>NumCar;
	fin>>TMax;
	fin>>Seed;
	fin>>TStep;
	fin>>line1;
	fin>>line2;

	//read input file data
	int i;
	while(fin>>i) //count 
	{
		fin>>y[i];
	}	
	
	//Analysis of data
	double lam;
	double chi;
	Chi(fout, y, i, lam, chi, TMax, TStep);

	//Print results to output file
	PrintRes(fout, TrackL, VMax, Prob, NumCar, TMax, Seed, lam, chi);	

	//close input & output files
	fin.close();
	fout.close();
}

//Open Input & Output files
void OpenFile(ifstream &fin, ofstream &fout)
{
	fin.open("time.out");	
	fout.open("chi.out");	
}

//Chi Square Analysis
void Chi(ofstream& fout, int *pobs, int xmax, double &lam,
double &chi, int TMax, int TStep)
{
	//calculate mean number of cars that lap per time step
	//(weighted average)
	double wsum=0;
	double psum=TMax/TStep; //number of counts
	for(int i=0; i<=xmax; i++)
	{
		wsum = i*pobs[i] + wsum;
	}
	lam = wsum/psum;

	//calculate standard deviations of each data set
	double sig1=0; //stdev of observed
	double sig2=0; //stdev of expected
	double vsum=0; //used to calculate variance
	for(int ct=0; ct<=TStep; ct++)
	{
		vsum=pow((ct-lam),2)*pobs[ct]+vsum;
	}
	sig1=pow(vsum/(psum-1),0.5);
	sig2=pow(lam,0.5);

	//generate array based on Poisson statistics
	//(using lambda above)
	double pexp[MAX]; //expected
	double expsum; //sum up to TStep
	for(int b=0; b<TStep; b++) 
	{
		pexp[b]=Poisson(lam,b)*psum;
		expsum=pexp[b]+expsum;
	}
	pexp[TStep]=psum-expsum;

	//calculate chi square value
	chi=0;
	for(int c=0; c<=TStep; c++)
	{
		cout<<c<<setw(10)<<pobs[c]<<setw(15)<<pexp[c]<<endl;
		
		if(pexp[c]!=0)
		{
			chi=chi+pow((pobs[c]-pexp[c]),2)/pexp[c];
		}
	}
	cout<<"Sigma"<<setw(10)<<sig1<<setw(15)<<sig2<<endl;	
	cout<<endl<<"LAMDA= "<<lam<<endl;
	cout<<"CHI SQUARED= "<<chi<<endl<<endl;
}

//Poisson algorithm (to avoid large number computations)
double Poisson(double lam, int k)
{
	double sum; //sum of natural log k=1 to k
	double lnp; //ln of poisson value
	double p; //poisson value

	sum=0;
	for(int i=1; i<=k; i++)
	{
		sum=sum+log(lam/i);
	}
	lnp=-lam+sum;
	p=pow(E,lnp);

	return p;

}

//Print results to output file
void PrintRes(ofstream &fout, int trackl, double vmax, double prob, 
int numcar, int tmax, int seed, double lam, double chi)
{
	fout<<"TrackLength: "<<trackl<<endl;
	fout<<"MaxVelocity: "<<vmax<<endl;
	fout<<"SlowDownProbability: "<<prob<<endl;
	fout<<"NumberOfCars: "<<numcar<<endl;
	fout<<"MaxTime: "<<tmax<<endl;
	fout<<"RandomSeed: "<<seed<<endl<<endl;

	fout<<"LAMBDA: "<<lam<<endl;
	fout<<"CHI-SQUARE: "<<chi<<endl;

}

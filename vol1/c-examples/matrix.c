#include "aifh-vol1.h"

/* The code in this file came from multiple sources. I made some modifications to allow
   these functions to play nice together.  Jeff Heaton - December 8, 2013.
http://rosettacode.org/wiki/QR_decomposition
http://math.nist.gov/javanumerics/jama/
*/

mat matrix_new(int m, int n)
{
	mat x;
	int i;

	x = (mat)malloc(sizeof(mat_t));
	x->v = (double**)malloc(sizeof(double) * m);
	x->v[0] = (double*)calloc(sizeof(double), m * n);
	for (i = 0; i < m; i++)
		x->v[i] = x->v[0] + n * i;
	x->m = m;
	x->n = n;
	return x;
}
 
void matrix_delete(mat m)
{
	free(m->v[0]);
	free(m->v);
	free(m);
}

mat matrix_copy(mat m) {
	mat result;
	unsigned int row, col;

	/* This is probably faster with multiple memcpy's */
	result = matrix_new(m->m,m->n);
	for(row=0;row<m->m;row++) {
		for(col=0;col<m->n;col++) {
			result->v[row][col] = m->v[row][col];
		}
	}
	return result;
}

	/**
	 * sqrt(a^2 + b^2) without under/overflow.
	 * 
	 * @param a
	 *            First param.
	 * @param b
	 *            Second param.
	 * @return The result.
	 */
static double calc_hypot(double a, double b) {
	double r;
	if (fabs(a) > fabs(b)) {
		r = b / a;
		r = fabs(a) * sqrt(1 + r * r);
	} else if (b != 0) {
		r = a / b;
		r = fabs(b) * sqrt(1 + r * r);
	} else {
		r = 0.0;
	}
	return r;
}


mat matrix_solve_qr(mat xMatrix, mat yMatrix) {
	mat qr,x,result;
	double *rDiag,s;
	int i,n,m,k,j,nx;

	/* Initial validations */
	if (xMatrix->m != yMatrix->m) {
		printf("Matrix row dimensions must agree.\n");
		exit(1);
	}

	/* Perform a QR decomp. */
	n = xMatrix->n;
	m = xMatrix->m;
	rDiag = (double*)calloc(n,sizeof(double));
	qr = matrix_copy(xMatrix);

	// Main loop.
	for (k = 0; k < n; k++) {
		// Compute 2-norm of k-th column without under/overflow.
		double nrm = 0;
		for (i = k; i < m; i++) {
			nrm = calc_hypot(nrm, qr->v[i][k]);
		}

		if (nrm != 0.0) {
			// Form k-th Householder vector.
			if (qr->v[k][k] < 0) {
				nrm = -nrm;
			}
			for (i = k; i < m; i++) {
				qr->v[i][k] /= nrm;
			}
			qr->v[k][k] += 1.0;

			// Apply transformation to remaining columns.
			for (j = k + 1; j < n; j++) {
				s = 0.0;
				for (i = k; i < m; i++) {
					s += qr->v[i][k] * qr->v[i][j];
				}
				s = -s / qr->v[k][k];
				for (i = k; i < m; i++) {
					qr->v[i][j] += s * qr->v[i][k];
				}
			}
		}
		rDiag[k] = -nrm;
	}

	/* Validate */
	for(i=0;i<n;i++) {
		if( fabs(rDiag[i])==0 ) {
			printf("Matrix is rank deficient. Data fails to converge.");
			exit(1);
		}
	}

	/* Now solve */

	/* Copy right hand side */
	nx = yMatrix->n;
	x = matrix_copy(yMatrix);

	/* Compute Y = transpose(Q)*B */
	for (k = 0; k < n; k++) {
		for (j = 0; j < nx; j++) {
			s = 0.0;
			for (i = k; i < m; i++) {
				s += qr->v[i][k] * x->v[i][j];
			}
			s = -s / qr->v[k][k];
			for (i = k; i < m; i++) {
				x->v[i][j] += s * qr->v[i][k];
			}
		}
	}
	/* Solve R*X = Y; */
	for (k = n - 1; k >= 0; k--) {
		for (j = 0; j < nx; j++) {
			x->v[k][j] /= rDiag[k];
		}
		for (i = 0; i < k; i++) {
			for (j = 0; j < nx; j++) {
				x->v[i][j] -= x->v[k][j] * qr->v[i][k];
			}
		}
	}

	/* Build result matrix */
	result = matrix_new(n,nx);
	for(i=0;i<n;i++) {
		for(j=0;j<nx;j++) {
			result->v[i][j] = x->v[i][j];
		}
	}

	/* Cleanup */
	matrix_delete(qr);
	matrix_delete(x);

	/* Return the result */
	return result;
}

static mat get_matrix(mat source, int *r, int r_len, int j0, int j1) {
	mat result;
	int i,j;

	result = matrix_new(r_len, j1 - j0 + 1);
		
	for (i = 0; i < r_len; i++) {
		for (j = j0; j <= j1; j++) {
			result->v[i][j - j0] = source->v[r[i]][j];
		}
	}
		
	return result;
}

mat matrix_solve_lu(mat xMatrix, mat yMatrix) {
	mat lu,x;
	double s,t;
	int i,n,m,k,j,nx,kmax,p;
	int *piv;
	int pivsign;
	double *LUrowi;
	double *LUcolj;

	/* Initial validations */
	if (xMatrix->m != yMatrix->m) {
		printf("Matrix row dimensions must agree.\n");
		exit(1);
	}

	// Use a "left-looking", dot-product, Crout/Doolittle algorithm.
	lu = matrix_copy(xMatrix);
	m = xMatrix->m;
	n = xMatrix->n;

	piv = (int*)calloc(m,sizeof(int));
		
	for (i = 0; i < m; i++) {
		piv[i] = i;
	}

	pivsign = 1;
	LUcolj = (double*)calloc(m,sizeof(double));

	// Outer loop.

	for (j = 0; j < n; j++) {

	// Make a copy of the j-th column to localize references.

		for (i = 0; i < m; i++) {
			LUcolj[i] = lu->v[i][j];
		}

	// Apply previous transformations.

		for (i = 0; i < m; i++) {
			LUrowi = lu->v[i];

			// Most of the time is spent in the following dot product.

			kmax = MIN(i, j);
			s = 0.0;
			for (k = 0; k < kmax; k++) {
				s += LUrowi[k] * LUcolj[k];
			}

			LUrowi[j] = LUcolj[i] -= s;
		}

		// Find pivot and exchange if necessary.

		p = j;
		for (i = j + 1; i < m; i++) {
			if (fabs(LUcolj[i]) > fabs(LUcolj[p])) {
				p = i;
			}
		}
		if (p != j) {
			for (k = 0; k < n; k++) {
				t = lu->v[p][k];
				lu->v[p][k] = lu->v[j][k];
				lu->v[j][k] = t;
			}
			k = piv[p];
			piv[p] = piv[j];
			piv[j] = k;
			pivsign = -pivsign;
		}

		// Compute multipliers.

		if (j < m && lu->v[j][j] != 0.0) {
			for (i = j + 1; i < m; i++) {
				lu->v[i][j] /= lu->v[j][j];
			}
		}
	}

	/* Validate */
	for(i=0;i<n;i++) {
		if( lu->v[i][i]==0 ) {
			printf("Matrix is rank deficient. Data fails to converge.");
			exit(1);
		}
	}

	/* Now solve */

		// Copy right hand side with pivoting
	nx = yMatrix->n;
	x = get_matrix(yMatrix,piv,m, 0, nx - 1);
		
	// Solve L*Y = B(piv,:)
	for (k = 0; k < n; k++) {
		for (i = k + 1; i < n; i++) {
			for (j = 0; j < nx; j++) {
				x->v[i][j] -= x->v[k][j] * lu->v[i][k];
			}
		}
	}

	// Solve U*X = Y;
	for (k = n - 1; k >= 0; k--) {
		for (j = 0; j < nx; j++) {
			x->v[k][j] /= lu->v[k][k];
		}
		for (i = 0; i < k; i++) {
			for (j = 0; j < nx; j++) {
				x->v[i][j] -= x->v[k][j] * lu->v[i][k];
			}
		}
	}

	/* Cleanup */
	free(piv);
	free(LUcolj);
	matrix_delete(lu);
	
	return x;
}
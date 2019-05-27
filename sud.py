sudoku = [
	[1,2,0,0,0,0,0,0,0],
	[2,0,0,0,0,0,0,0,0],
	[0,0,0,3,0,0,0,0,0],
	[0,0,0,0,0,4,0,0,0],
	[0,0,0,0,0,0,8,0,0],
	[3,0,0,0,0,0,0,6,0],
	[5,0,0,7,0,0,0,0,0],
	[0,0,0,0,0,1,0,0,0],
	[0,0,2,0,0,0,0,0,0],
]


def solve (sudoku, val):
	for i in range(len(sudoku)):
		for j in range(len(sudoku[0])):
			sudoku[i][j] = val
			if valido(sudoku):
				solve(sudoku, val)

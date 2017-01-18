function lcs(s1,s2)
  A=[]
  for c in s1
    push!(A,c)
  end
  B=[]
  for c in s2
    push!(B,c)
  end
  n = length(A)
  m = length(B)
  dptable = Array{Int64,2}(n+1,m+1)
  for i = 0:n
    for j = 0:m
      if i == 0 || j == 0
        dptable[i+1,j+1] = 0
      elseif A[i] == B[j]
        dptable[i+1,j+1] = dptable[i,j] + 1
      else
        dptable[i+1,j+1] = 0
      end
    end
  end
  len = -1
  x=1
  y=1
  for i = 1:n+1
    for j in 1:m+1
      if dptable[i,j] > len
        len=dptable[i,j]
        x=i
        y=j
      end
    end
  end
  seq = Array{Char,1}(len)
  seq=A[x-len:x-1]
  join(seq)
end

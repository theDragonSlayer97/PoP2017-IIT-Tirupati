function hanoi(ndisks, startPeg = 1, endPeg = 3)
  if ndisks>0
    hanoi(ndisks-1, startPeg, 6-startPeg-endPeg)
    print("Move disk")
    print(" from peg ")
    print(startPeg)
    print(" to peg ")
    println(endPeg)
    hanoi(ndisks-1, 6-startPeg-endPeg, endPeg)
  end
end

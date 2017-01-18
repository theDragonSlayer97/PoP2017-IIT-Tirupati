function combsort(list)
  gap = length(list)
  swaps = true
  while gap > 1 || swaps
    gap = Int(max(1, fld(gap, 1.25)))
    swaps = false
    for i = 1:length(list)-gap
      j = i+gap
      if list[i] > list[j]
        list[i], list[j] = list[j], list[i]
        swaps = true
      end
    end
  end
  return list
end

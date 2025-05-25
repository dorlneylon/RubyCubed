def factorial n
    f = 1
    
    while n > 0 do
        f = f * n
        n = n - 1
    end
    
    return f
end

# prints factorial of 10
puts factorial(10)

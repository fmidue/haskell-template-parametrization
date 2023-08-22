module Solution where

-- Give datatype definitions for types A, B, C and D, (just) such that
-- the values given below become valid values of the respective types.
-- Do not introduce any additional datatypes besides A, B, C and D.

data #{t} = #{d} Bool #{t} | #{c} (Either (Int, Int) ())
data B = #{b} (Maybe #{v})
data #{v} = #{g} Int [Bool] #{t} | #{e} #{v} | #{f}
data #{w} = #{a} [#{v}] (Int, Bool)

value1 :: B
value1 = #{b} (Just (#{e} #{f}))

value2 :: #{t2}
value2 = #{v2}

value3 :: #{t3}
value3 = #{v3}

value4 :: #{w}
value4 = #{a} [#{e} (#{g} #{int3} [] (#{d} False (#{c} (Left (#{int3}, #{int2}))))), #{f}] (#{int1}, True)

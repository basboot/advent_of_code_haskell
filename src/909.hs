main :: IO ()

data Value = I Int | F (Int -> Int)

instance Show Value where
    show (I x) = show x
    show (F _) = "F <function>"  -- Indicate it's a function without trying to show it


a :: Value -> Value
a x = x + 1
a (F _) _ _ = error "The first argument must be an integer."


z :: Value -> Value -> Value
z _ v = v

-- s(u)(v)(w) -> v(u(v)(w))
s :: Value -> Value -> Value -> Value
s (F u) (F v) w = v $ u v w
s (I _) _ _ = error "The first argument must be a function."
s _ (I _) _ = error "The second argument must be a function."



main = do
  putStrLn $ show $ s (F z) (F a) (I 0)
```hs
VBinary OpApp (VBinary OpApp (VLam (Name {unName = 1}) (VBinary OpApp (VLam (Name {unName = 2}) (VBinary OpApp (VVar (Name {unName = 1})) (VBinary OpApp (VVar (Name {unName = 2})) (VVar (Name {unName = 2}))))) (VLam (Name {unName = 2}) (VBinary OpApp (VVar (Name {unName = 1})) (VBinary OpApp (VVar (Name {unName = 2})) (VVar (Name {unName = 2}))))))) (VLam (Name {unName = 3}) (VLam (Name {unName = 4}) (VIf (VBinary OpLT (VVar (Name {unName = 4})) (VInt 2)) (VInt 1) (VBinary OpAdd (VBinary OpApp (VVar (Name {unName = 3})) (VBinary OpSub (VVar (Name {unName = 4})) (VInt 1))) (VBinary OpApp (VVar (Name {unName = 3})) (VBinary OpSub (VVar (Name {unName = 4})) (VInt 2)))))))) (VInt 40)
```

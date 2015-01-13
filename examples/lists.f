List = λα::*.∀φ::*⇒*.φ α → (α → φ α → φ α) → φ α;

nil = Λα::*.Λφ::*⇒*.λnil:φ α.λcons:α → φ α → φ α.nil;

cons = Λα::*.λx:α.λxs:List α.
         Λφ::*⇒*.λnil:φ α.λcons:α → φ α → φ α.
             cons x (xs [φ] nil cons);

fold = Λα::*.ΛB::*.
        λc:α → B → B.λn:B.λl:List α.
           l [λX::*.B] n c;

append = Λα::*.
          λl:List α.λr:List α.
            fold [α] [List α] (cons [α]) l r;

concat = Λα::*.
          λl:List (List α).
            fold [List α] [List α] (append [α]) (nil [α]) l;


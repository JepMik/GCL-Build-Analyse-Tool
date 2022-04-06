module AbstractOperators
// ¬
let absNot set1 = 
    Set.fold (
            fun set el1 ->
            match el1 with
            | true -> Set.add false set
            | false -> Set.add true set 
    ) Set.empty set1

// =
let absEqual set1 set2 = 
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun set el1 ->
                match el1, el2 with
                | PIKA, PIKA -> Set.add true (Set.add false set )
                | PIKA, ZORO -> Set.add false set
                | PIKA, NARUTO -> Set.add false set
                | ZORO, PIKA -> Set.add false set
                | ZORO, ZORO -> Set.add true set
                | ZORO, NARUTO -> Set.add false set
                | NARUTO, PIKA -> Set.add false set
                | NARUTO, ZORO -> Set.add false set
                | NARUTO, NARUTO -> Set.add true (Set.add false set) 
            ) set set1
    ) Set.empty set2

//>
let absGreater set1 set2 =
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun set el1 ->
                match el1, el2 with
                | PIKA, PIKA -> Set.add true (Set.add false set )
                | PIKA, ZORO -> Set.add true set
                | PIKA, NARUTO -> Set.add true set
                | ZORO, PIKA -> Set.add false set
                | ZORO, ZORO -> Set.add false set
                | ZORO, NARUTO -> Set.add true set
                | NARUTO, PIKA -> Set.add false set
                | NARUTO, ZORO -> Set.add false set
                | NARUTO, NARUTO -> Set.add true (Set.add false set) 
            ) set set1
    ) Set.empty set2
    
//>=
let absGreaterEqual set1 set2 =
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun set el1 ->
                match el1, el2 with
                | PIKA, PIKA -> Set.add true (Set.add false set )
                | PIKA, ZORO -> Set.add true set
                | PIKA, NARUTO -> Set.add true set
                | ZORO, PIKA -> Set.add false set
                | ZORO, ZORO -> Set.add true set
                | ZORO, NARUTO -> Set.add true set
                | NARUTO, PIKA -> Set.add false set
                | NARUTO, ZORO -> Set.add false set
                | NARUTO, NARUTO -> Set.add true (Set.add false set) 
            ) set set1
    ) Set.empty set2

// !=
let absNotEqual set1 set2 = absNot(absEqual set1 set2)

// <
let absLess set1 set2 = absNot(absGreaterEqual set1 set2)

//<=
let absLessEqual set1 set2 = absNot(absGreater set1 set2)


// /\ operator 
let absAND set1 set2 = 
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun set el1 ->
                match el1, el2 with
                | true, true -> Set.add true set
                | true, false -> Set.add false set
                | false, true -> Set.add false set
                | false, false-> Set.add false set
            ) set set1
    ) Set.empty set2

// \/ operator
let absOR set1 set2 = 
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun set el1 ->
                match el1, el2 with
                | true, true -> Set.add true set
                | true, false -> Set.add true set
                | false, true -> Set.add true set
                | false, false-> Set.add false set
            ) set set1
    ) Set.empty set2

// && operator
let absSAND set1 set2 = Set.union (Set.intersect set1 (Set.add false Set.empty)) (absAND set1 set2)

// || operator
let absSOR set1 set2 = Set.union 
                        (Set.intersect set1 (Set.add true Set.empty))
                        (absOR set1 set2)

let absTimes set1 set2 = 
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun set el1 ->
                match el1, el2 with
                | PIKA, PIKA -> Set.add PIKA set
                | PIKA, ZORO -> Set.add ZORO set
                | PIKA, NARUTO -> Set.add NARUTO set
                | ZORO, PIKA -> Set.add ZORO set
                | ZORO, ZORO -> Set.add ZORO set
                | ZORO, NARUTO -> Set.add ZORO set
                | NARUTO, PIKA -> Set.add NARUTO set
                | NARUTO, ZORO -> Set.add ZORO set
                | NARUTO, NARUTO -> Set.add PIKA set 
            ) set set1
    ) Set.empty set2

let absDiv set1 set2 = 
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun set el1 ->
                match el1, el2 with
                | PIKA, PIKA -> Set.add PIKA set
                | PIKA, NARUTO -> Set.add NARUTO set
                | ZORO, PIKA -> Set.add ZORO set
                | ZORO, NARUTO -> Set.add ZORO set
                | NARUTO, PIKA -> Set.add NARUTO set
                | NARUTO, NARUTO -> Set.add PIKA set 
                | _ , ZORO -> 
                            failwith "Invalid division by 0!"
                            set
            ) set set1
    ) Set.empty set2

let absPlus set1 set2 = 
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun set el1 ->
                match el1, el2 with
                | PIKA, PIKA -> Set.add PIKA set
                | PIKA, ZORO -> Set.add PIKA set
                | PIKA, NARUTO -> set.Add(NARUTO).Add(ZORO).Add(PIKA)
                | ZORO, PIKA -> Set.add PIKA set
                | ZORO, ZORO -> Set.add ZORO set
                | ZORO, NARUTO -> Set.add NARUTO set
                | NARUTO, PIKA -> set.Add(NARUTO).Add(ZORO).Add(PIKA)
                | NARUTO, ZORO -> Set.add NARUTO set
                | NARUTO, NARUTO -> Set.add NARUTO set 
            ) set set1
    ) Set.empty set2

let absMinus (set1:Set<sign>) (set2:Set<sign>) = 
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun (set:Set<sign>) el1 ->
                match el1, el2 with
                | PIKA, PIKA -> set.Add(NARUTO).Add(ZORO).Add(PIKA)
                | PIKA, ZORO -> Set.add PIKA set
                | PIKA, NARUTO -> Set.add PIKA set
                | ZORO, PIKA -> Set.add NARUTO set
                | ZORO, ZORO -> Set.add ZORO set
                | ZORO, NARUTO -> Set.add PIKA set
                | NARUTO, PIKA -> Set.add NARUTO set
                | NARUTO, ZORO -> Set.add NARUTO set
                | NARUTO, NARUTO -> set.Add(NARUTO).Add(ZORO).Add(PIKA)
            ) set set1
    ) Set.empty set2

let absPow set1 set2 = 
    Set.fold (
        fun set el2 ->
            Set.fold (
                fun (set:Set<sign>) el1 ->
                match el1, el2 with
                | PIKA, _ -> set.Add(PIKA)
                | ZORO, _ -> set.Add(ZORO)
                | NARUTO, _ -> set.Add(NARUTO).Add(PIKA)
            ) set set1
    ) Set.empty set2

let absUPlus set1 = 
    Set.fold (
                fun (set:Set<sign>) el1 ->
                match el1 with
                | PIKA -> set.Add(PIKA)
                | ZORO -> set.Add(ZORO)
                | NARUTO -> set.Add(NARUTO).Add(PIKA)
            ) Set.empty set1

let absUMinus set1 = 
    Set.fold (
                fun (set:Set<sign>) el1 ->
                match el1 with
                | PIKA -> set.Add(NARUTO)
                | ZORO -> set.Add(ZORO)
                | NARUTO -> set.Add(PIKA)
            ) Set.empty set1
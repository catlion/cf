import Control.Monad
import Data.List
import qualified Data.Map as M

formatsol :: (Maybe Int, Int) -> String
formatsol (Nothing, _) = "Brand new problem!"
formatsol (Just simil, idx) =
    show idx ++ "\n[:" ++ (replicate simil '|') ++ ":]"

solve :: String -> [String] -> String
solve lesha archive =
    formatsol $ similarity l archive
    where l = words lesha

invcount src tgt =
    fst $ foldl cmp (0,1) tgti
    where srci = M.fromList $ zip src [1..]
          tgti = Data.List.map (\x -> (x, M.lookup x)) srci
          cmp _ (_,Nothing) = fail "Fuuuuu"
          cmp (str, sidx) (cnt, Just idx) =
            if idx < sidx then (cnt+1, idx+1)
            else (cnt, idx+1)

similarity :: [String] -> [String] -> (Maybe Int, Int)
similarity lesha arch =
    foldl chk (Nothing, 1) afixed
    where afixed = map (drop 1 . words) arch
          chk atask (x, c) =
            if atask `elem` lperm then
                let n = length lesha
                    s = n * (n-1) / 2 - inv + 1
                    inv = invcount atask lesha in
                case x of Nothing -> (Just s, c + 1)
                          Just os -> if s > os then (Just s, c + 1)
                                     else (x, c + 1)
            else (x, c + 1)
          lperm = permutations lesha

getInt x = read x :: Int

main = do
    leshawcount <- getLine
    lesha <- getLine
    archnum <- fmap getInt getLine
    arch <- replicateM archnum getLine
    putStrLn $ solve lesha arch

{-
Широко известный в узких кругах белорусский олимпиадник Леша решил немного подзаработать, чтобы купить себе квартиру площадью на один квадратный метр больше. Для этого он хочет составить и провести Super Rated Match (SRM) на сайте Torcoder.com. Но вот беда — суровый торкодерский координатор Иван не принимает ни одной Лешиной задачи, называя каждую обидным словом «боян». И вот после очередной предложенной задачи дело чуть не дошло до взаимной обиды.

Вам предлагается выступить в роли справедливого судьи и определить, действительно ли задача является настолько инновационно новой, как уверен в этом Леша, или все-таки Иван прав, и похожая задача уже встречалась в прошедших SRM.

Вам даны описания Лешиной задачи и задач из архива сайта Torcoder.com. Описание каждой задачи представляет собой последовательность слов. При этом гарантируется, что слова в Лешиной задаче не повторяются, в то время как описание задачи из архива может содержать произвольное количество повторяющихся слов.

«Похожесть» Лешиной задачи на некоторую задачу из архива определим следующим образом. Среди всех перестановок слов предлагаемой задачи выберем ту, которая встречается в задаче из архива в качестве подпоследовательности. Если таких перестановок несколько, выберем ту, число инверсий в которой минимально. Тогда «похожесть» задачи можно записать в виде , где n — количество слов в задаче Леши, а x — количество инверсий в выбранной перестановке. Обратите внимание, что «похожесть» p — положительное число.

Задачу назовем инновационно новой, если среди задач из архива Ивана не найдется ни одной задачи, которая содержит в себе в качестве подпоследовательности некоторую перестановку слов Лешиной задачи.

Рассудите ребят, определив, является ли предлагаемая задача новой, либо указав задачу из архива, которая больше всего напоминает Лешину задачу.
Входные данные

Первая строка содержит число n (1 ≤ n ≤ 4) — количество слов в Лешиной задаче. Во второй строке находятся n слов, разделенных пробелом — краткое описание задачи.

Третья строка содержит число m (1 ≤ m ≤ 10) — количество задач в архиве Torcoder.com. Следующие m строк содержат описания задач в формате «k s1 s2 ... sk», где k (1 ≤ k ≤ 20) — количество слов в задаче, а si — слово задачи.

Слова из описаний всех задач содержат не более 10 строчных латинских букв.
Выходные данные

В случае, если Лешина задача является инновационно новой, выведите строку «Brand new problem!» (без кавычек).

В противном случае в первой строке выведите номер задачи из архива, наиболее похожей на Лешину задачу. Если таких задач несколько, выведите ту, номер которой минимальный. Во второй строке выведите строку из символов [:, символа |, повторенного p раз, и символов :], где p — «похожесть» этой задачи на Лешину. Задачи нумеруются начиная с единицы в том порядке, в котором они заданы во входных данных. 
-}
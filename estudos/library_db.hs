import Data.Data (Data)
import Data.Char

type Person = String
type Book = String
type Database = [(Person, Book)]

books :: Database -> Person -> [Book]
books db findPerson = [book | (person,book) <- db, person == findPerson]

borrowers ::  Database -> Book -> [Person]
borrowers db findBook = [person | (person,book) <- db, book == findBook]

bookBorrowed :: Database -> Book -> Bool
bookBorrowed db findBook = not (null [book | (person, book) <- db, book == findBook])


booksNumber :: Database -> Person -> Int
booksNumber db findPerson = length [1 | (person, _) <- db, person == findPerson]

makeLoan :: Database -> Person -> Book -> Database
makeLoan db person book = (person, book) : db

returnLoan :: Database -> Person -> Book -> Database
returnLoan db person book = [(p, b) | (p, b) <- db, (p, b) /= (person, book)]
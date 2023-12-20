# Library Management System

This project implements a library management system developed in Haskell. It provides a command-line interface that allows users to perform various operations, such as creating, modifying, and deleting libraries, as well as managing books, members, and transactions within a selected library.

## Estructura del Proyecto
app: Contains the Main.hs file, serving as the entry point for the application.
src: Holds the source code organized into modules, such as Data for data definitions, and Utils for utility functions.
library: Stores text files (BookDB.txt, MemberDB.txt, TransactionDB.txt) acting as databases for books, members, and transactions.
LibraryManagementSystem.cabal: Configuration file for the Haskell build system.
LICENSE: Information about the project's license.
README.md: Documentation providing an overview of the project and instructions for use.

```
.
├── app
│   └── Main.hs
├── src
│   ├── Data
│   │   ├── Book.hs
│   │   │   - Defines the Book data structure and related functions.
│   │   ├── Library.hs
│   │   │   - Contains the core functionality for managing the library.
│   │   ├── Member.hs
│   │   │   - Defines the Member data structure and associated operations.
│   │   ├── Transactions.hs
│   │   │   - Handles transactions and interactions with the library.
│   └── Utils.hs
│       - Houses utility functions used across the application.
├── library
│   ├── BookDB.txt
│   │   - Text file storing information about books in the library.
│   ├── MemberDB.txt
│   │   - Text file containing data about library members.
│   └── TransactionDB.txt
│       - Text file storing transaction history in the library.
├── LibraryManagementSystem.cabal
│   - Configuration file for the Haskell build system.
├── LICENSE
│   - Licensing information for the project.
└── README.md
    - Documentation providing an overview of the project and instructions for use.
```

# Key Features
Creation, modification, and deletion of libraries.
Book management: adding, removing, and modifying book information.
Member management: adding, removing, and modifying member information.
Transaction management: borrowing and returning books.
# Ejecución del Programa

```
$ stack build
```

# Collaboration:
Contributions and suggestions are welcome. If you encounter any issues or have ideas for improvements, please open an issue or submit a pull request.

License:
This project is under the **MIT License**.

Thank you for using our Haskell library management system!
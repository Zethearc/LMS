# Library Management System

Este proyecto es una implementación simple de un sistema de gestión de bibliotecas en Haskell. Permite gestionar libros y miembros de una biblioteca, realizando operaciones como añadir libros, añadir miembros, prestar y devolver libros, entre otras.

## Estructura del Proyecto
El proyecto está organizado en varios módulos para facilitar la comprensión y mantenimiento del código:

```
├── app
│   └── Main.hs
├── src
│   ├── Data
│   │   ├── Book.hs
│   │   ├── Library.hs
│   │   └── Member.hs
│   ├── Utils.hs
│   
├── test
│   └── MemberTest.hs
├── LibraryManagementSystem.cabal
├── LICENSE
└── README.md
```

# Uso del Programa
Al ejecutar la aplicación, se crea una biblioteca vacía. Luego, se pueden añadir libros y miembros, realizar operaciones como préstamos y devoluciones, y actualizar la información de libros y miembros.

# Ejecución del Programa

```
$ stack build
```

# Operaciones Disponibles
* Añadir Libro: Se pueden añadir libros a la biblioteca.
* Añadir Miembro: Se pueden añadir nuevos miembros a la biblioteca.
* Actualizar Información de Libro/Miembro: Permite modificar la información de un libro o miembro existente.
* Prestar/Devolver Libro: Gestiona las operaciones de préstamo y devolución de libros.
* Eliminar Libro/Miembro: Elimina un libro o miembro de la biblioteca.

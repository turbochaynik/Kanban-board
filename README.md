# Kanban Monomer

## Project Description

Kanban Monomer is a desktop Kanban board application written in Haskell
using the Monomer GUI library.

The application allows a user to create, edit, delete, move and inspect
tasks on a simple board with three columns:

- To Do
- In Progress
- Done

The current state of the board can also be saved to a JSON file and loaded
again on the next application start.

---

## Implemented Features

The project supports the following features:

- creation of a new task;
- editing of an existing task;
- deletion of a task;
- moving a task between board columns;
- viewing task details;
- validation of input data;
- saving board state to `kanban.json`;
- loading saved board state from `kanban.json`.

Each task contains:

- title;
- description;
- type;
- tags;
- assignee.

---

## Technologies

The project is implemented in:

- Haskell
- Monomer
- Aeson
- Lens

---

## Project Structure

The project contains the following main files and directories:

- `src/Model.hs` — application data model;
- `src/Update.hs` — event handling and update logic;
- `src/View.hs` — user interface;
- `app/Main.hs` — application entry point;
- `kanban.json` — saved board state file;
- `README.md` — project description and usage instructions.

---

## Build and Run

### Using Cabal

Build the project:

```bash
cabal build


<img align="right" src="http://mario.net.au/images/toyrob.jpg" width="120">

# Toy R.O.B

Haskell version of the Toy Robot challenge.

## Usage

    $ stack build && stack exec ToyRot

Run commands from a file:

    $ stack build && stack exec ToyRot < commands.txt


## Assumptions / Features

- Multiple valid PLACE commands will allow for Toy R.O.B. to demonstrate his powers of teleportation


## Specification

The original specification for the 'Toy Robot' challenge below, with the following modifications:

- Less screaming, all commands are title case, i.e. `Move`, `Left`, `Right`, etc.
- Input format adjusted to take advantage of built-in `Read` derivations, e.g. `Place (1,2,North)`


### Description
- The application is a simulation of a toy robot moving on a square tabletop, of dimensions 5 units x 5 units.
- There are no other obstructions on the table surface.
- The robot is free to roam around the surface of the table, but must be prevented from falling to destruction. Any movement
that would result in the robot falling from the table must be prevented, however further valid movement commands must still
be allowed.

*Create an application that can read in commands of the following form:*

```
PLACE X,Y,F
MOVE
LEFT
RIGHT
REPORT
```

- `PLACE` will put the toy robot on the table in position `X,Y` and facing `NORTH`, `SOUTH`, `EAST` or `WEST`.
- The origin (`0,0`) can be considered to be the `SOUTH WEST` most corner.
- The first valid command to the robot is a `PLACE` command, after that, any sequence of commands may be issued, in any order, including another `PLACE` command. The application should discard all commands in the sequence until a valid `PLACE` command has been executed
- `MOVE` will move the toy robot one unit forward in the direction it is currently facing.
- `LEFT` and `RIGHT` will rotate the robot 90 degrees in the specified direction without changing the position of the robot.
- `REPORT` will announce the `X`,`Y` and `F` of the robot. This can be in any form, but standard output is sufficient.
- A robot that is not on the table can choose to ignore the <code>MOVE</code>, <code>LEFT</code>, <code>RIGHT</code> and <code>REPORT</code> commands.
- Input can be from a file, or from standard input, as the developer chooses.
- Provide test data to exercise the application.


### Constraints
The toy robot must not fall off the table during movement. This also includes the initial placement of the toy robot.
Any move that would cause the robot to fall must be ignored.

Example Input and Output:

```
PLACE 0,0,NORTH
MOVE
REPORT
 => 0,1,NORTH
```

```
PLACE 0,0,NORTH
LEFT
REPORT
 => 0,0,WEST
```

```
PLACE 1,2,EAST
MOVE
MOVE
LEFT
MOVE
REPORT
 => 3,3,NORTH
```

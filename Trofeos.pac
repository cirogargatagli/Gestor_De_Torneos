| package |
package := Package name: 'Trofeos'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #AbstractEquipo;
	add: #Equipo;
	add: #EquipoProxy;
	add: #EquiposModel;
	add: #EquipoTorneo;
	add: #Jugador;
	add: #MenuEquipo;
	add: #MenuEquipos;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'Core\Object Arts\Dolphin\Base\Dolphin'
	'Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #AbstractEquipo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #EquiposModel
	instanceVariableNames: 'equipos'
	classVariableNames: 'Instancia'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Jugador
	instanceVariableNames: 'nombre apellido dni'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MenuEquipo
	instanceVariableNames: 'equipo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MenuEquipos
	instanceVariableNames: 'model'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractEquipo subclass: #Equipo
	instanceVariableNames: 'id nombre jugadores copas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AbstractEquipo subclass: #EquipoProxy
	instanceVariableNames: 'id nombre equipo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EquipoProxy subclass: #EquipoTorneo
	instanceVariableNames: 'puntos partidosGanados partidosPerdidos partidosEmpatados golesAFavor golesEnContra'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

AbstractEquipo guid: (GUID fromString: '{b7da764d-e1fe-469d-85b6-48beb38c85e5}')!
AbstractEquipo comment: ''!
!AbstractEquipo categoriesForClass!Kernel-Objects! !
!AbstractEquipo methodsFor!

agregarJugador: unJugador!

eliminarJugador: unJugador!

imprimir!

iniEquipo: unNombre id: unId!

modiCopas: numCopas!

verCopas!

verId!

verJugadores!

verNombre! !
!AbstractEquipo categoriesFor: #agregarJugador:!public! !
!AbstractEquipo categoriesFor: #eliminarJugador:!public! !
!AbstractEquipo categoriesFor: #imprimir!public! !
!AbstractEquipo categoriesFor: #iniEquipo:id:!public! !
!AbstractEquipo categoriesFor: #modiCopas:!public! !
!AbstractEquipo categoriesFor: #verCopas!public! !
!AbstractEquipo categoriesFor: #verId!public! !
!AbstractEquipo categoriesFor: #verJugadores!public! !
!AbstractEquipo categoriesFor: #verNombre!public! !

EquiposModel guid: (GUID fromString: '{1e67eb96-086b-46dc-8df2-dca0d63c9448}')!
EquiposModel comment: ''!
!EquiposModel categoriesForClass!Kernel-Objects! !
!EquiposModel methodsFor!

agregarEquipo: unEquipo
	equipos add: unEquipo.!

cargarEquipos
	|eq1 eq2 eq3 eq4 eq5 eq6|
	 
	eq1 := Equipo crearEquipo: 'Racing Club' id: 'RAC'.
	eq1 agregarJugador: (Jugador crearJugadorDni: 30564201 nombre: 'Lisandro' apellido: 'Lopez').
	eq1 agregarJugador: (Jugador crearJugadorDni: 34526845 nombre: 'Luciano' apellido: 'Vietto').
	eq1 agregarJugador: (Jugador crearJugadorDni: 39520145 nombre: 'Rodrigo' apellido: 'De Paul').
	eq1 agregarJugador: (Jugador crearJugadorDni: 29533125 nombre: 'Marcelo' apellido: 'Díaz').
	eq1 agregarJugador: (Jugador crearJugadorDni: 40205122 nombre: 'Nicolás' apellido: 'Reniero').
	eq1 agregarJugador: (Jugador crearJugadorDni: 38451251 nombre: 'Benjamín' apellido: 'Garré').

	eq2 := Equipo crearEquipo: 'Independiente' id: 'IND'.
	eq2 agregarJugador: (Jugador crearJugadorDni: 35945159 nombre: 'Silvio' apellido: 'Romero').
	eq2 agregarJugador: (Jugador crearJugadorDni: 35128498 nombre: 'Fabricio' apellido: 'Bustos').
	eq2 agregarJugador: (Jugador crearJugadorDni: 38498481 nombre: 'Ezequiel' apellido: 'Muñoz').
	eq2 agregarJugador: (Jugador crearJugadorDni: 32849845 nombre: 'Alan' apellido: 'Franco').
	eq2 agregarJugador: (Jugador crearJugadorDni: 31800894 nombre: 'Lucas' apellido: 'Romero').
	eq2 agregarJugador: (Jugador crearJugadorDni: 34641842 nombre: 'Alexander' apellido: 'Barboza').

	eq3 := Equipo crearEquipo: 'Boca Juniors' id: 'BOC'.
	eq3 agregarJugador: (Jugador crearJugadorDni: 29564462 nombre: 'Carlos' apellido: 'Tevez').
	eq3 agregarJugador: (Jugador crearJugadorDni: 42312698 nombre: 'Edwin' apellido: 'Cardona').
	eq3 agregarJugador: (Jugador crearJugadorDni: 34519213 nombre: 'Sebastián' apellido: 'Villa').
	eq3 agregarJugador: (Jugador crearJugadorDni: 32580921 nombre: 'Eduardo' apellido: 'Salvio').
	eq3 agregarJugador: (Jugador crearJugadorDni: 36498011 nombre: 'Mauro' apellido: 'Zárate').
	eq3 agregarJugador: (Jugador crearJugadorDni: 32109749 nombre: 'Esteban' apellido: 'Andrada').

	eq4 := Equipo crearEquipo: 'River Plate' id: 'RIV'.
	eq4 agregarJugador: (Jugador crearJugadorDni: 32498125 nombre: 'Lucas' apellido: 'Pratto').
	eq4 agregarJugador: (Jugador crearJugadorDni: 35189498 nombre: 'Franco' apellido: 'Armani').
	eq4 agregarJugador: (Jugador crearJugadorDni: 32328491 nombre: 'Jorge' apellido: 'Carrascal').
	eq4 agregarJugador: (Jugador crearJugadorDni: 32111145 nombre: 'Enzo' apellido: 'Perez').
	eq4 agregarJugador: (Jugador crearJugadorDni: 29115521 nombre: 'Gonzalo' apellido: 'Montiel').
	eq4 agregarJugador: (Jugador crearJugadorDni: 30529842 nombre: 'Javier' apellido: 'Pinola').

	eq5 := Equipo crearEquipo: 'San Lorenzo' id: 'SLO'.
	eq5 agregarJugador: (Jugador crearJugadorDni: 42151952 nombre: 'Ignacio' apellido: 'Piatti').
	eq5 agregarJugador: (Jugador crearJugadorDni: 40460012 nombre: 'Óscar' apellido: 'Romero').
	eq5 agregarJugador: (Jugador crearJugadorDni: 39811512 nombre: 'Marcelo' apellido: 'Herrera').
	eq5 agregarJugador: (Jugador crearJugadorDni: 31198281 nombre: 'Matías' apellido: 'Palacios').
	eq5 agregarJugador: (Jugador crearJugadorDni: 29542831 nombre: 'Fabricio' apellido: 'Coloccini').
	eq5 agregarJugador: (Jugador crearJugadorDni: 31998720 nombre: 'Nicolás' apellido: 'Fernandez').


	eq6 := Equipo crearEquipo: 'Huracán' id: 'HUR'.
	eq6 agregarJugador: (Jugador crearJugadorDni: 35898105 nombre: 'Diego' apellido: 'Mercado').
	eq6 agregarJugador: (Jugador crearJugadorDni: 32688920 nombre: 'Saúl' apellido: 'Salcedo').
	eq6 agregarJugador: (Jugador crearJugadorDni: 30529842 nombre: 'César' apellido: 'Ibañez').
	eq6 agregarJugador: (Jugador crearJugadorDni: 36544218 nombre: 'Norberto' apellido: 'Briasco').
	eq6 agregarJugador: (Jugador crearJugadorDni: 39554122 nombre: 'Andrés' apellido: 'Chavez').
	eq6 agregarJugador: (Jugador crearJugadorDni: 30529842 nombre: 'Facundo' apellido: 'Cambeses').

	equipos add: eq1.
	equipos add: eq2.
	equipos add: eq3.
	equipos add: eq4.
	equipos add: eq5.
	equipos add: eq6.!

eliminarEquipo: unEquipo
	equipos remove: unEquipo.!

iniEquiposModel
	equipos := OrderedCollection new.
	self cargarEquipos.!

obtenerEquipo: unId
	^(equipos detect: [:e | e verId = unId] ifNone: [^nil]).!

verEquipos
	^equipos.! !
!EquiposModel categoriesFor: #agregarEquipo:!public! !
!EquiposModel categoriesFor: #cargarEquipos!public! !
!EquiposModel categoriesFor: #eliminarEquipo:!public! !
!EquiposModel categoriesFor: #iniEquiposModel!public! !
!EquiposModel categoriesFor: #obtenerEquipo:!public! !
!EquiposModel categoriesFor: #verEquipos!public! !

!EquiposModel class methodsFor!

eliminarInstancia
	Instancia := nil.!

obtenerInstancia
	(Instancia = nil)
		ifTrue: [
			Instancia := (self new) iniEquiposModel.
		].
	
	^Instancia.! !
!EquiposModel class categoriesFor: #eliminarInstancia!public! !
!EquiposModel class categoriesFor: #obtenerInstancia!public! !

Jugador guid: (GUID fromString: '{e40daad7-051f-44ca-a254-97d3f72b0b6f}')!
Jugador comment: ''!
!Jugador categoriesForClass!Kernel-Objects! !
!Jugador methodsFor!

imprimir
	^(nombre, ' ', apellido, ', DNI: ', dni printString).!

iniJugadorDni: unDni nombre: unNombre apellido: unApellido
	nombre := unNombre.
	apellido := unApellido.
	dni := unDni.!

modiApellido: unApellido
	apellido := unApellido.!

modiNombre: unNombre
	nombre := unNombre.!

verApellido
	^apellido.!

verDni
	^dni.!

verNombre
	^nombre! !
!Jugador categoriesFor: #imprimir!public! !
!Jugador categoriesFor: #iniJugadorDni:nombre:apellido:!public! !
!Jugador categoriesFor: #modiApellido:!public! !
!Jugador categoriesFor: #modiNombre:!public! !
!Jugador categoriesFor: #verApellido!public! !
!Jugador categoriesFor: #verDni!public! !
!Jugador categoriesFor: #verNombre!public! !

!Jugador class methodsFor!

crearJugadorDni: unDni nombre: unNombre apellido: unApellido
	^(self new) iniJugadorDni: unDni nombre: unNombre apellido: unApellido.! !
!Jugador class categoriesFor: #crearJugadorDni:nombre:apellido:!public! !

MenuEquipo guid: (GUID fromString: '{63bc09cd-215c-4325-9d35-fe3042516687}')!
MenuEquipo comment: ''!
!MenuEquipo categoriesForClass!Kernel-Objects! !
!MenuEquipo methodsFor!

agregarJugador
	|nombre apellido dni duplicado jugador|
	
	Transcript
		show: 'Ingresando un nuevo jugador al equipo';
		cr.
	
	nombre := Prompter prompt: 'Ingrese un nombre: '.
	(nombre = nil | nombre isEmpty)
		ifTrue: [
			Transcript
				show: 'No se ha ingresado un nombre válido';
				cr.
			^(-1).
		].

	apellido := Prompter prompt: 'Ingrese un apellido: '.
	(apellido = nil | apellido isEmpty)
		ifTrue: [
			Transcript
				show: 'No se ha ingresado un DNI válido';
				cr.
			^(-1).
		].

	dni := (Prompter prompt: 'Ingrese un DNI: ') asNumber.
	(dni = nil)
		ifTrue: [
			Transcript
				show: 'No se ha ingresado un apellido válido';
				cr.
			^(-1).
		].

	duplicado := equipo verJugadores detect: [:each | each verDni = dni] ifNone: [duplicado := nil].
	(duplicado ~= nil)
		ifTrue: [
			Transcript
				show: 'Ya existe un jugador con el mismo DNI, intente nuevamente';
				cr.
			^(-1).
		].

	jugador := Jugador crearJugadorDni: dni nombre: nombre apellido: apellido.
	equipo agregarJugador: jugador.

	Transcript
		show: ('El jugador (', jugador imprimir, ') ha sido añadido');
		cr.!

eliminarJugador
	|dni jugador|

	Transcript
		show: 'Los jugadores que puede eliminar son: ';
		cr.
	self mostrarJugadores.
	
	dni := (Prompter prompt: 'Ingrese el DNI del jugador que desea eliminar: ') asNumber.
	(dni = nil)
		ifTrue: [
			Transcript
				show: 'No se ha ingresado un DNI válido';
				cr.
		].
	
	jugador := equipo verJugadores detect: [:each | each verDni = dni].
	
	Transcript
		show: '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~';
		cr.
	(jugador ~= nil)
		ifTrue: [
			equipo eliminarJugador: jugador.
		
			Transcript
				show: ('El jugador (', jugador imprimir, ') ha sido eliminado');
				cr.
		]
		ifFalse: [
			Transcript
				show: 'No se ha podido eliminar el jugador';
				cr.
		].!

imprimirEquipoActual
	Transcript
		show: ('El equipo que se está editando es: (', equipo imprimir, ')');
		cr.!

imprimirJugadores
	Transcript
		show: 'Los jugadores que tiene el equipo son: ';
		cr.
	self mostrarJugadores.!

iniMenu: unEquipo
	equipo := unEquipo.!

mostrarJugadores
	|jugadores i|
	jugadores := equipo verJugadores.
	
	i := 1.
	jugadores do: [:jugador | 
		Transcript
			show: ('Jugador ', i printString, ': ', jugador imprimir);
			cr.
		i := i + 1.
	].!

mostrarMenu
	|o opc m|
	
	m := [
		Transcript
			clear;
			show: '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~';
			cr;
			show: '+-+ MENÚ EQUIPO +-+';
			cr;
			show: '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~';
			cr;
			show: ('+-+ MODIFICANDO EQUIPO: ', equipo verNombre, ' +-+');
			cr;
			show: '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~';
			cr;
			show: 'Las opciones del menú son: ';
			cr;
			show: '0- Volver al menú de equipos';
			cr;
			show: '1- Agregar un jugador';
			cr;
			show: '2- Eliminar un jugador';
			cr;
			show: '3- Listar los jugadores';
			cr;
			show: '4- Ver el equipo que se está editando';
			cr;
			show: '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~';
			cr.
	].
	
	opc := 5.
	[opc ~= 0] 
		whileTrue: [
			m value.

			(opc = 1) ifTrue: [self agregarJugador].
			(opc = 2) ifTrue: [self eliminarJugador].
			(opc = 3) ifTrue: [self imprimirJugadores].
			(opc = 4) ifTrue: [self imprimirEquipoActual].

			opc := (Prompter prompt: 'Ingrese la opción: ') asNumber.
			(opc = nil)
				ifTrue: [
					opc := 0.
				]
		].
! !
!MenuEquipo categoriesFor: #agregarJugador!public! !
!MenuEquipo categoriesFor: #eliminarJugador!public! !
!MenuEquipo categoriesFor: #imprimirEquipoActual!public! !
!MenuEquipo categoriesFor: #imprimirJugadores!public! !
!MenuEquipo categoriesFor: #iniMenu:!public! !
!MenuEquipo categoriesFor: #mostrarJugadores!private! !
!MenuEquipo categoriesFor: #mostrarMenu!public! !

!MenuEquipo class methodsFor!

crearMenu: unEquipo
	^(self new) iniMenu: unEquipo.! !
!MenuEquipo class categoriesFor: #crearMenu:!public! !

MenuEquipos guid: (GUID fromString: '{32f9c06b-bfe3-429a-8268-e0c86f80b14b}')!
MenuEquipos comment: ''!
!MenuEquipos categoriesForClass!Kernel-Objects! !
!MenuEquipos methodsFor!

agregarEquipo
	|nombre id duplicado equipo|
		
	Transcript
		show: 'Añadiendo un nuevo equipo';
		cr.

	nombre := Prompter prompt: 'Ingrese el nombre del equipo: '.
	(nombre = nil | nombre isEmpty)
		ifTrue: [
			Transcript
				show: 'No se ha ingresado un nombre válido';
				cr.
			^(-1).
		].

	id := Prompter prompt: 'Ingrese la abreviatura del equipo: '.
	(id = nil | id isEmpty)
		ifTrue: [
			id := (nombre copyFrom: 1 to: 3) asUppercase.
		].
	id := id asUppercase.

	duplicado := model verEquipos detect: [:each | each verId = id] ifNone: [duplicado := nil].
	(duplicado ~= nil)
		ifTrue: [
			Transcript
				show: 'La abreviatura del equipo ya existe, intente nuevamente';
				cr.
			^(-1).
		].

	equipo := Equipo crearEquipo: nombre id: id.
	model agregarEquipo: equipo.

	Transcript
		show: ('El equipo (', equipo imprimir, ') ha sido añadido');
		cr.!

eliminarEquipo
	|id equipo|

	Transcript
		show: 'Los equipos que se pueden eliminar son:';
		cr.
	self mostrarEquipos.
	
	id := Prompter prompt: 'Ingrese la abreviatura del equipo que desea eliminar: '.
	Transcript
		show: '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~';
		cr.
	(id = nil | id isEmpty)
		ifTrue: [
			Transcript
				show: 'No se ha ingresado una abreviatura válida';
				cr.
			^(-1).
		].
	id := id asUppercase.

	equipo := model verEquipos detect: [:each | each verId = id] ifNone: [equipo := nil]. 
	(equipo ~= nil)
		ifTrue: [
			model eliminarEquipo: equipo.
				
			Transcript
				show: ('El equipo (', equipo imprimir, ') se ha eliminado');
				cr.
		]
		ifFalse: [
			Transcript
				show: 'El equipo no se ha podido eliminar';
				cr.
		].!

imprimirEquipos
	Transcript
		show: 'Los equipos disponibles son: ';
		cr.
	self mostrarEquipos.!

iniMenu
	model := EquiposModel obtenerInstancia.!

modificarEquipo
	|id equipo|

	Transcript
		show: 'Los equipos que se pueden modificar son:';
		cr.
	self mostrarEquipos.

	id := Prompter prompt: 'Ingrese la abreviatura del equipo que desea modificar: '.
	(id = nil | id isEmpty)
		ifTrue: [
			Transcript
				show: 'No se ha ingresado una abreviatura válida';
				cr.
			^(-1).
		].
	id := id asUppercase.
	
	equipo := model verEquipos detect: [:each | each verId = id].

	(equipo ~= nil)
		ifTrue: [
			|menu|
			
			menu := MenuEquipo crearMenu: equipo.	
			menu mostrarMenu.
		]
		ifFalse: [
			Transcript
				show: 'No se ha encontrado el equipo';
				cr.
		].!

mostrarEquipos
	|equipos i|
		
	equipos := model verEquipos.
	
	i := 1.
	equipos do: [:equipo | 
		Transcript
			show: ('Equipo ', i printString, ': ', equipo imprimir);
			cr.
		i := i + 1.
	].!

mostrarMenu
	|o opc m|
	
	m := [
		Transcript
			clear;
			show: '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~';
			cr;
			show: '+-+ MENÚ EQUIPOS +-+';
			cr;
			show: '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~';
			cr;
			show: 'Las opciones del menú son: ';
			cr;
			show: '0- Volver al menú principal';
			cr;
			show: '1- Agregar un equipo';
			cr;
			show: '2- Eliminar un equipo';
			cr;
			show: '3- Modificar un equipo';
			cr;
			show: '4- Listar todos los equipos';
			cr;
			show: '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~';
			cr.
	].

	opc := 5.
	[opc ~= 0] 
		whileTrue: [
			m value.

			(opc = 1) ifTrue: [self agregarEquipo].
			(opc = 2) ifTrue: [self eliminarEquipo].
			(opc = 3) ifTrue: [self modificarEquipo. m value.].
			(opc = 4) ifTrue: [self imprimirEquipos].

			opc := (Prompter prompt: 'Ingrese la opción: ') asNumber.
			(opc = nil)
				ifTrue: [
					opc := 0.
				]
		].
! !
!MenuEquipos categoriesFor: #agregarEquipo!public! !
!MenuEquipos categoriesFor: #eliminarEquipo!public! !
!MenuEquipos categoriesFor: #imprimirEquipos!public! !
!MenuEquipos categoriesFor: #iniMenu!public! !
!MenuEquipos categoriesFor: #modificarEquipo!public! !
!MenuEquipos categoriesFor: #mostrarEquipos!private! !
!MenuEquipos categoriesFor: #mostrarMenu!public! !

!MenuEquipos class methodsFor!

crearMenu
	^(self new) iniMenu.! !
!MenuEquipos class categoriesFor: #crearMenu!public! !

Equipo guid: (GUID fromString: '{fe6db9e3-e693-46e2-809f-685495b9a3d6}')!
Equipo comment: ''!
!Equipo categoriesForClass!Kernel-Objects! !
!Equipo methodsFor!

agregarJugador: unJugador
	jugadores add: unJugador.!

eliminarJugador: unJugador
	jugadores remove: unJugador.!

imprimir
	^(nombre, ', ', id).!

iniEquipo: unNombre id: unId
	nombre := unNombre.
	id := unId.
	jugadores := OrderedCollection new.
	copas := 0.!

modiCopas: numCopas
	copas := numCopas.!

verCopas
	^copas.!

verId
	^id.!

verJugadores
	^jugadores.!

verNombre
	^nombre.! !
!Equipo categoriesFor: #agregarJugador:!public! !
!Equipo categoriesFor: #eliminarJugador:!public! !
!Equipo categoriesFor: #imprimir!public! !
!Equipo categoriesFor: #iniEquipo:id:!public! !
!Equipo categoriesFor: #modiCopas:!public! !
!Equipo categoriesFor: #verCopas!public! !
!Equipo categoriesFor: #verId!public! !
!Equipo categoriesFor: #verJugadores!public! !
!Equipo categoriesFor: #verNombre!public! !

!Equipo class methodsFor!

crearEquipo: unNombre id: unId
	^(self new) iniEquipo: unNombre id: unId.! !
!Equipo class categoriesFor: #crearEquipo:id:!public! !

EquipoProxy guid: (GUID fromString: '{52c3fdc8-5c8f-4860-95c4-402dd501ee82}')!
EquipoProxy comment: ''!
!EquipoProxy categoriesForClass!Kernel-Objects! !
!EquipoProxy methodsFor!

agregarJugador: unJugador
	(equipo ~= nil)
		ifTrue: [
			 self iniciarEquipo.
		].

	equipo agregarJugador: unJugador.!

eliminarJugador: unJugador
	(equipo ~= nil)
		ifTrue: [
			 self iniciarEquipo.
		].
	
	equipo eliminarJugador: unJugador.!

imprimir
	^(nombre, ', ', id).!

iniciarEquipo
	equipo := Equipo crearEquipo: nombre id: id.!

iniEquipo: unNombre id: unId
	nombre := unNombre.
	id := unId.
	equipo := nil.!

modiCopas: numCopas
	(equipo ~= nil)
		ifTrue: [
			 self iniciarEquipo.
		].

	equipo modiCopas: numCopas.!

verCopas
	(equipo ~= nil)
		ifTrue: [
			 self iniciarEquipo.
		].
	
	^(equipo verCopas).!

verId
	^id.!

verJugadores
	^(equipo verJugadores).!

verNombre
	^nombre.! !
!EquipoProxy categoriesFor: #agregarJugador:!public! !
!EquipoProxy categoriesFor: #eliminarJugador:!public! !
!EquipoProxy categoriesFor: #imprimir!public! !
!EquipoProxy categoriesFor: #iniciarEquipo!private! !
!EquipoProxy categoriesFor: #iniEquipo:id:!public! !
!EquipoProxy categoriesFor: #modiCopas:!public! !
!EquipoProxy categoriesFor: #verCopas!public! !
!EquipoProxy categoriesFor: #verId!public! !
!EquipoProxy categoriesFor: #verJugadores!public! !
!EquipoProxy categoriesFor: #verNombre!public! !

!EquipoProxy class methodsFor!

crearEquipo: unNombre id: unId
	^(self new) iniEquipo: unNombre id: unId.! !
!EquipoProxy class categoriesFor: #crearEquipo:id:!public! !

EquipoTorneo guid: (GUID fromString: '{68ff808d-15bf-449c-8ddd-ae0d41bbd30e}')!
EquipoTorneo comment: ''!
!EquipoTorneo categoriesForClass!Kernel-Objects! !
!EquipoTorneo methodsFor!

iniEquipoTorneo: unNombre id: unId
	nombre := unNombre.
	id := unId.
	equipo := nil.
	puntos := 0.
	partidosGanados := 0.
	partidosPerdidos := 0.
	partidosEmpatados := 0.
	golesAFavor := 0.
	golesEnContra := 0.! !
!EquipoTorneo categoriesFor: #iniEquipoTorneo:id:!public! !

!EquipoTorneo class methodsFor!

crearEquipoTorneo: unNombre id: unId
	^(self new) iniEquipoTorneo: unNombre id: unId.! !
!EquipoTorneo class categoriesFor: #crearEquipoTorneo:id:!public! !

"Binary Globals"!


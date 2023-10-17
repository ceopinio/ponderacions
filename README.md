# Ponderacions del Centre d'Estudis d'Opinió

Aquest repositori conté el codi per calcular les ponderacions que s'usen en les enquestes del Centre d'Estudis d'Opinió, CEO d'ara endavant. Aquestes permeten que la mostra d'una enquesta sigui representativa de la població objectiu.

## Estructura del repositori

- `enquestes` conté diferents subcarpetes que cada una fa referència a un estudi diferent. Per diferenciar-ho s'ha fet servir el número que l'estudi té en el Registre d'Estudis d'Opinió (REO). Per consultar diferents estudis podeu anar al següent [enllaç](https://ceo.gencat.cat/es/estudis/registre-estudis-dopinio/).  
- `dta` conté les dades poblacionals per realitzar les ponderacions.
- `R` conté scripts d'R on es defineixen funcions que s'usen durant el codi de les ponderacions.

## Estructura de les branques (*branch*)

El repositori té dues branques (*branch*) perquè les enquestes que es realitzen de forma autoadministrada poden respondre en línia (mitjançant l'ordinador, tauleta o telèfon intel·ligent) o en paper. Tot i que els qüestionaris són el màxim semblants possibles, algunes preguntes no apareixen en el qüestionari en paper, i per tal d'obtenir la mostra representativa en les preguntes que només es responen en línia, es duen a terme les ponderacions a partir de les persones que han respost en línia.

- `main` conté el codi de les ponderacions de les preguntes que s'han respost en línia i en paper.
- `ponderacions-online` conté el codi de les ponderacions per a les preguntes que només s'han contestat en línia.

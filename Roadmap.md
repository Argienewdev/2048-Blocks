# Proyect: <span style="color:#e6538a">2048 - Blocks</span>
*Lógica para Cs. de la Computación - 2025*

## Roadmap

- Add a new type of notification:
    - This notification must fade in on the header and fade out so as not to disturb the games' continuity.
- Add a "singularity" effect to the fired block so it looks like it pops out and the new block pops in.
- Always load hints using animation times and cache the shoot info so as to use said info when the user finally shoots.
- Add a slight delay to the invasive pop ups so as not to have them instantly appear after they can be invoked.
- Make the interface responsive.



## In process

# Shoot cache system

Init shoots and caches all possible outcomes at start.
The back end seems to work but I still have to check for correctness.
The front end is yet to use the cached info.
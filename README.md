# Een gebruiker wegwijs maken doorheen een applicatie van aanzienlijke omvang

> Work in progress 👨🏻‍💻

**HoGent Faculteit Bedrijf en Organisatie**

Promotor: Karine Samyn

Co-promotor: Bert Maurau ([Cardify](https://getcardify.com))

Academiejaar: 2019-2020

## Aan de slag

### Vereisten

0. Zorg ervoor dat LaTeX geinstalleerd is.

### Compiling

1. Clone deze repo.

   ```bash
   git clone https://github.com/JakobLierman/bachelorproef-hogent-1920
   ```

2. Open de project root directory.

   ```bash
   cd bachelorproef-hogent-1920
   ```

3. Maak het bash script uitvoerbaar.

   ```bash
   chmod +x ./compile_all_tex.sh
   ```

4. Voer het script uit. Dit script compileert alle `.tex`-bestanden.

   ```sh
   ./compile_all_tex.sh
   ```

5. De gecompileerde PDF-bestanden zijn te vinden in de subdirectories.

   > Optioneel: open de bestanden op macOS.
   > 
   > ```sh
   > open bachproef/bachproef-tin.pdf
   > open voorstel/lierman_jakob_voorstel.pdf
   > open poster/conference_poster.pdf
   > ```

   > Optioneel: open de bestanden op Linux.
   > 
   > ```sh
   > xdg-open bachproef/bachproef-tin.pdf
   > xdg-open voorstel/lierman_jakob_voorstel.pdf
   > xdg-open poster/conference_poster.pdf
   > ```

   

>  Voor de luiaards: kopieer dit in de terminal. 😴
>  
> ```bash
> git https://github.com/JakobLierman/bachelorproef-hogent-1920; cd bachelorproef-hogent-1920; chmod +x ./compile_all_tex.sh; ./compile_all_tex.sh
> ```

## Repository layout

- *bachproef*
  De bachelorproef zelf.
- *voorstel*
  Het bachelorproef voorstel.
- *poster*
  Poster om een snel overzicht te krijgen over de inhoud van de bachelorproef.

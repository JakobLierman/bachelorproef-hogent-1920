# Een gebruiker wegwijs maken doorheen een applicatie van aanzienlijke omvang

**HoGent Faculteit Bedrijf en Organisatie**

Promotor: Karine Samyn

Co-promotor: Bert Maurau ([Cardify](https://getcardify.com))

Academiejaar: 2019-2020

## Abstract

Elke gebruiker moet leren werken met de applicaties die hij of zij op zijn of haar toestel installeert. Men kan deze gebruiker helpen door middel van implementatie van learnability-elementen doorheen de software. De gebruiker kan op weg gezet worden door middel van een onboarding wanneer hij of zij de applicatie voor de eerste maal opent.

De grote meerderheid aan bedrijven en ontwikkelaars hebben echter de tijd of middelen niet om deze technieken zo geoptimaliseerd mogelijk uit te werken. Zo zijn deze vaak druk bezet met het halen van strakke deadlines om nieuwe functionaliteiten op punt te hebben dat de gebruikservaring vaak wat achterwege gelaten wordt.

Deze scriptie geeft duidelijkheid over de verschillende soorten onboarding en help-elementen en hoe deze best te gebruiken om een positief effect te hebben bij de eindgebruiker. Er wordt ook besproken hoe men best de implementatie van deze technieken test aan de hand van usability testing. Dit schrijven werd aangevuld met inzichten van ontwikkelaars.

Om het effect van learnability-elementen te meten werd er een usability test uitgevoerd met enkele participanten. Uit dit onderzoek resulteert dat het gebruik van deze elementen wel degelijk voordelig is, als en slechts als deze op de juiste manier verwerkt zijn doorheen de software. Elke software verschilt en voor elke software moet opnieuw bekeken worden waar men best hulp kan voorzien. Dit gaat vlot aan de hand van usability tests doorheen fases van de ontwikkeling.

De relatie tussen (het gebrek aan) in-app user training en de gebruiksduur en/of levensduur van de applicatie wordt aanbevolen als toekomstig onderzoek. Hierbij kan men deze proef als startpunt gebruiken met een gerichte applicatie en een steekproef waarbij elke participant interesse toont in de functionaliteiten van de applicatie.

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

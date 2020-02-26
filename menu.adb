--Yann

with Arbre_Genealogique;     use Arbre_Genealogique;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Registre;               use Registre;

procedure Menu is

    procedure Recuperer_Reponse(Reponse : out Integer; Min : in Integer; Max : in Integer) is
    begin
        -- On récupére un entier compris entre Min et Max saisi par l'utilisateur
        Get(Reponse);
        New_Line;

        -- Si cette réponse n'est pas comprise entre 1 et 4, on redemande à l'utilisateur
        while Reponse > Max and Reponse < Min loop
            Put_Line("Veuillez saisir un ENTIER compris entre " & Integer'Image(Min) & " et " & Integer'Image(Max) & "."); New_Line;
            Get(Reponse);
            New_Line;
        end loop;
    end Recuperer_Reponse;


    procedure Initialiser_Programme (Arbre : out T_Arbre_Gen; Reg : out T_Registre) is
        Jour_Naissance : Integer; -- Jour de naissance du premier individu (on s'en sert pour construire sa clé)
        Mois_Naissance : Integer; -- Mois de naissance du premier individu (on s'en sert pour construire sa clé)
        Annee_Naissance : Integer; -- Année de naissance du premier individu (on s'en sert pour construire sa clé)
        Nom : unbounded_string; -- Nom du premier individu
        Prenom : unbounded_string; -- Prenom du premier individu
        Sexe : Character; -- Sexe du premier individu
    begin
        Put_Line("Bonsoir, bienvenue dans le gestionnaire d'arbre généalogique.");
        New_Line;

        Put_Line("Veuillez indiquer le premier individu à l'origine de votre arbre généalogique :");
        Put_Line("Renseignez son nom : ");
        Nom := To_Unbounded_String (Get_Line); New_Line;
        Put_Line("Renseignez son prénom : ");
        Prenom := To_Unbounded_String (Get_Line); New_Line;
        Put_Line("Renseignez son jour de naissance : ");
        Recuperer_Reponse(Jour_Naissance, 1, 32);
        Put_Line("Renseignez son mois de naissance : ");
        Recuperer_Reponse(Mois_Naissance, 1, 12);
        Put_Line("Renseignez son année de naissance : ");
        Recuperer_Reponse(Annee_Naissance, 0, 2050);
        loop
            Put_Line("Renseignez son sexe (en rentrant M ou F en majuscule) : ");
            Get(Sexe);
            exit when Sexe = 'F' or Sexe = 'M';
        end loop;

        Initialiser_Gen (Arbre, Reg, Nom, Prenom, Jour_Naissance, Mois_Naissance, Annee_Naissance, Sexe);
    end Initialiser_Programme;

    procedure Menu_Modifier (Arbre : in out T_Arbre_Gen; Reg : in out T_Registre; Continuer : in out Boolean) is
        Reponse : Integer; -- Répones au choix de l'action
        Cle : Integer; -- Clé d'un individu
        Jour_Naissance : Integer; -- Jour de naissance
        Mois_Naissance : Integer; -- Mois de naissance
        Annee_Naissance : Integer; -- Année de naissance
        Nom : unbounded_string; -- Nom d'un parent
        Prenom : unbounded_string; -- Prenom d'un parent
        Sexe : Character; -- Sexe d'un parent
    begin
        -- On affiche ce que peut modifier l'utilisateur grace aux procedures d'arbre_généalogique
        Put_Line("Menu 'Modifier', que voulez-vous faire ?"); New_Line;
        Put_Line("1. Ajouter un père à une personne."); New_Line;
        Put_Line("2. Ajouter une mère à une personne."); New_Line;
        Put_Line("3. Supprimer un ancêtre."); New_Line;
        Put_Line("4. Quitter le menu 'Modifier'"); New_Line;

        Recuperer_Reponse(Reponse, 1, 4);

        -- On veut ajouter un parent
        if Reponse = 1 or Reponse = 2 then
            Put_Line("[AJOUT D'UN PARENT]"); New_Line; New_Line;
            -- On veut ajouter un père
            if Reponse = 1 then
                Put_Line("A qui voulez-vous ajouter un père ? (veuillez saisir la clé de la personne qui disponible lors de l'affichage de l'arbre)"); New_Line;
                -- On sait que le sexe est 'M' !
                Sexe := 'M';
            -- On veut ajouter une mère
            else
                Put_Line("A qui voulez-vous ajouter une mère ? (veuillez saisir la clé de la personne qui disponible lors de l'affichage de l'arbre)"); New_Line;
                -- On sait que le sexe est 'F' !
                Sexe := 'F';
            end if;
            -- On récupére la clé de l'enfant
            Get(Cle);

            -- On récupére quelques données sur le parent
            Put_Line("Veuillez renseigner quelques informations sur ce parent :"); New_Line;
            Put_Line("Indiquez son nom : ");
            Skip_Line;
            Nom := To_Unbounded_String (Get_Line); New_Line;
            New_Line;
            Put_Line("Indiquez son prénom : ");
            Prenom := To_Unbounded_String (Get_Line); New_Line;
            New_Line;
            Put_Line("Indiquez son jour de naissance : ");
            Recuperer_Reponse(Jour_Naissance, 1, 31);
            New_Line;
            Put_Line("Indiquez son mois de naissance : ");
            Recuperer_Reponse(Mois_Naissance, 1, 12);
            New_Line;
            Put_Line("Indiquez son année de naissance : ");
            Recuperer_Reponse(Annee_Naissance, 0, 2050);
            New_Line;
            
            -- On ajoute ce parent à l'arbre
            Ajouter_Parent (Arbre, Reg, Cle, Nom, Prenom, Jour_Naissance, Mois_Naissance, Annee_Naissance, Sexe);
            Put_Line("Parent bien ajouté à l'arbre !"); New_Line;
        
        -- On veut supprimer un ancêtre
        elsif Reponse = 3 then
            Put_Line("[SUPPRIMER UN INDIVIDU]"); New_Line; New_Line;
            -- On récupére la clé d'un individu à supprimer;
            Put_Line("Veuillez saisir la clé de l'individu à supprimer (disponible lors de l'affichage de l'arbre) : ");
            Get(Cle);
            New_Line;
            Supprimer(Arbre, Reg, Cle);
        -- On veut sortir du menu
        elsif Reponse = 4 then
            Continuer := False;
        else
            null;
        end if;

        -- Traitement des exceptions
        exception
        when Cle_Presente_Exception_Reg => Put_Line("Il y a eu plus de 100 naissances ce jour là !? Oulala..."); New_Line; Put_Line("Tu ne peux plus ajouter de personnes né ce jour là."); New_Line;
        when Cle_Absente_Exception_Reg => Put_Line("Cette clé n'existe pas, veuillez vérifier à l'aide de l'affichage de l'arbre"); New_Line; Continuer := False;
        when Cle_Presente_Exception_Gen => Put_Line("Il y a eu plus de 100 naissances ce jour là !? Oulala..."); New_Line; Put_Line("Tu ne peux plus ajouter de personnes né ce jour là."); New_Line;
        when Cle_Absente_Exception_Gen => Put_Line("Cette clé n'existe pas, veuillez vérifier à l'aide de l'affichage de l'arbre."); New_Line; Continuer := False;
        when Emplacement_Reserve_Exception_Gen => Put_Line("Cette personne a déjà ce parent ! Impossible d'en ajouter un nouveau."); New_Line; Continuer := False;
        when Constraint_Error => Put_Line("Veuillez rentrez un entier pour les clés");
        when others => Put_Line("Quelque chose n'a pas fonctionné, vous retournez au menu principal."); New_Line; Continuer := False;
    end Menu_Modifier;


    procedure Menu_Trouver(Arbre : in out T_Arbre_Gen; Reg : in out T_Registre; Continuer : in out Boolean) is
        Reponse : Integer; -- Réponse au choix de l'action
        Cle : Integer; -- Cle d'un individu
        Cle2 : Integer; -- Deuxième clé pour un individu
        Generation : Integer; -- Génération
    begin
        -- On affiche ce que peut trouver l'utilisateur grace aux procedures d'arbre_généalogique
        Put_Line("Menu 'Trouver', que voulez-vous faire ?"); New_Line;
        Put_Line("1. Obtenir le nombre d'ancêtres à partir d'une personne."); New_Line;
        Put_Line("2. Afficher les ancêtres d'une certaine génération."); New_Line;
        Put_Line("3. Afficher tous les ancêtres à partir d'une certaine génération."); New_Line;
        Put_Line("4. Afficher les ancêtres homonymes communs à deux personnes."); New_Line;
        Put_Line("5. Afficher les personnes orphelines."); New_Line;
        Put_Line("6. Afficher les personnes monoparentales"); New_Line;
        Put_Line("7. Afficher les personnes biparentales"); New_Line;
        Put_Line("8. Quitter le menu 'Trouver'"); New_Line;

        Recuperer_Reponse(Reponse, 1, 8);
        
        -- On veut obtenir le nombre d'ancêtres à partir d'une personne
        if Reponse = 1 then
            -- On récupére la clé de la personne à partir de laquelle compter le nombre d'ancêtres
            Put_Line("Saisir la clé de la personne à partir de laquelle compter le nombre d'ancêtres (disponible lors de l'affichage de l'arbre) : ");
            Get(Cle);
            New_Line;
            -- On affiche le nombre d'ancêtres
            Put_Line("Cette personne a" & Integer'Image(Nombre_Ancetre (Arbre, Cle)) & " ancêtres connus."); New_Line; New_Line;

        -- On veut afficher les ancêtres d'une certaine généation
        elsif Reponse = 2 then
            Put_Line("Saisir la clé de la personne à partir de laquelle on va compter la génération (disponible lors de l'affichage de l'arbre) : ");
            Get(Cle);
            New_Line;
            Put_Line("Saisir la génération dont vous souhaitez afficher les ancêtres : ");
            Recuperer_Reponse(Generation, 1, 2000);
            New_Line;
            -- On affiche les ancêtres
            Ancetre_Generation_X(Arbre, Cle, Generation);

        -- On veut afficher tous les ancêtres à partir d'une certaine génération
        elsif Reponse = 3 then
            Put_Line("Saisir la clé de la personne à partir de laquelle on va afficher les ancêtres (disponible lors de l'affichage de l'arbre) : ");
            Get(Cle);
            New_Line;
            Put_Line("Saisir le nombre de générations que vous voulez afficher : ");
            Recuperer_Reponse(Generation, 1, 2000);
            New_Line;
            -- On affiche tous les ancêtres
            Ancetres_Sur_N_Generations (Arbre, Cle, Generation);

        -- On veut afficher les ancêtres homonymes de deux personnes
        elsif Reponse = 4 then
            Put_Line("Saisir la clé de la première personne (disponible lors de l'affichage de l'arbre) : ");
            Get(Cle);
            New_Line;
            Put_Line("Saisir la clé de la deuxième personne (disponible lors de l'affichage de l'arbre) : ");
            Get(Cle2);
            New_Line;
            -- On affiche les ancêtres homonymes
            Ancetres_Homonymes (Arbre, Reg, Cle, Cle2);

        -- On veut afficher les personnes orphelines
        elsif Reponse = 5 then
            Put_Line("Les personnes sans parent enregistré sont :"); New_Line;
            -- On affiche les personnes orphelines
            Afficher_Orphelins(Arbre);
            
        -- On veut afficher les personnes monoparentales
        elsif Reponse = 6 then
            Put_Line("Les personnes avec un seul parent enregistré sont :"); New_Line;
            -- On affiche les personnes monoparentales
            Afficher_Monoparental(Arbre);
            null;
        -- On veut afficher les personnes biparentales
        elsif Reponse = 7 then
            Put_Line("Les personnes avec deux parents enregistrés sont :"); New_Line;
            -- On affiche les personnes biparentales
            Afficher_Biparental(Arbre);
            null;
        -- On veut quitter le menu 'Trouver'
        elsif Reponse = 8 then
            Continuer := False;
        else
            null;
        end if;

        -- Traitement des exceptions
        exception
        when Cle_Presente_Exception_Reg => Put_Line("Il y a eu plus de 100 naissances ce jour là !? Oulala..."); New_Line; Put_Line("Tu ne peux plus ajouter de personnes né ce jour là."); New_Line;
        when Cle_Absente_Exception_Reg => Put_Line("Cette clé n'existe pas, veuillez vérifier à l'aide de l'affichage de l'arbre"); New_Line; Continuer := False;
        when Cle_Presente_Exception_Gen => Put_Line("Il y a eu plus de 100 naissances ce jour là !? Oulala..."); New_Line; Put_Line("Tu ne peux plus ajouter de personnes né ce jour là."); New_Line;
        when Cle_Absente_Exception_Gen => Put_Line("Cette clé n'existe pas, veuillez vérifier à l'aide de l'affichage de l'arbre."); New_Line; Continuer := False;
        when Emplacement_Reserve_Exception_Gen => Put_Line("Cette personne a déjà ce parent ! Impossible d'en ajouter un nouveau."); New_Line; Continuer := False;
        when Constraint_Error => Put_Line("Veuillez rentrez un entier pour les clés");
        when others => Put_Line("Quelque chose n'a pas fonctionné, vous retournez au menu principal."); New_Line; Continuer := False;
    end Menu_Trouver;


    Continuer_Mod : Boolean; -- Continuer de modifier l'arbre
    Continuer_Trouver : Boolean; -- Continuer de trouver des informations dans l'arbre
    Aff : Boolean; -- Afficher l'arbre
    Quitter : Boolean; -- Quitter le programme 'menu'
    Reponse : Integer; -- Contient la réponse de l'utilisateur
    Attente : Integer; -- Variable qui ne sert qu'à mettre en pause le programme jusqu'à ce que l'utilisateur entre un caractère
    Arbre : T_Arbre_Gen;
    Reg : T_Registre;

begin
    -- On dit 'soir et on demande de rentrer le premier individu à l'origine de l'arbre
    Initialiser_Programme(Arbre, Reg);

    -- On initialise un booléen pour quitter le programme
    Quitter := False;
    loop
        -- On affiche les possibilités du menu.
        Put_Line("Que voulez vous faire ?"); New_Line;
        Put_Line("1. Afficher mon arbre."); New_Line;
        Put_Line("2. Trouver des informations dans mon arbre."); New_Line;
        Put_Line("3. Modifier mon arbre."); New_Line;
        Put_Line("4. Quitter."); New_Line;

        -- On récupére un entier compris entre 1 et 4 saisi par l'utilisateur
        Recuperer_Reponse(Reponse, 1, 4);

        -- On remet toutes les anciennes variables à False 'au cas où'
        Aff := False;
        Continuer_Trouver := False;
        Continuer_Mod := False;

        -- On change les variables pour savoir dans quel sous-menu on entre.
        case Reponse is
            when 1 => Aff := True;
            when 2 => Continuer_Trouver := True;
            when 3 => Continuer_Mod := True;
            when 4 => Quitter := True;
            when others => null;
        end case;

        -- Le premier sous-menu 'Afficher' n'en est pas un : il affiche seulement l'arbre
        if Aff then
            -- On lance la procedure qui affiche l'arbre
            Afficher_Arbre_Genealogique(Arbre); New_Line; New_Line;
            Afficher(Reg);
            -- On met à False la variable
            Aff := False;

            New_Line;
            Put_Line("Entrez n'importe quel chiffre pour continuer"); New_Line;
            Get(Attente);
            New_Line;
        end if;


        -- Affiche le sous-menu 'Trouver'
        while Continuer_Trouver loop
            Menu_Trouver(Arbre, Reg, Continuer_Trouver);
        end loop;

        -- Affiche le sous-menu 'Modifier'
        while Continuer_Mod loop
            Menu_Modifier(Arbre, Reg, Continuer_Mod);
        end loop;
        
        exit when Quitter = True;
    end loop;

    -- Vider l'arbre
    Vider(Arbre, Reg);
    Put_Line("Merci d'avoir utilisé notre logiciel."); New_Line;
end Menu;
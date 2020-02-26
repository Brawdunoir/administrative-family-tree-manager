procedure Trouver_Position (Arbre : in T_Pointeur; Cle : in T_Cle; Position : in out T_Pointeur)  is
    begin
        if Arbre.Cle = Cle then
            Position := Arbre;
        elsif Arbre = null then
            null;
        else
            Trouver_Position (Arbre.all.Fils_Gauche, Cle, Position);
            Trouver_Position (Arbre.all.Fils_Droite, Cle, Position);
        end if;
    end Trouver_Position;


procedure Rechercher_Position (Arbre : in T_Pointeur; Cle : in T_Cle; Pointeur : out T_Pointeur) is
    begin
        Pointeur := null;
        Trouver_Position (Arbre, Cle, Pointeur);

        if Pointeur = null then
            raise Cle_Asbente;
        else
            null;
        end if;
    end Rechercher_Position;


-- Dans le .ads de Arbre_binaire
procedure Ajouter_Fils_Droit (Arbre : in out T_Pointeur; Cle_Parent : in T_Cle; Cle_Fils : in T_Cle) is
        Pointeur : T_Pointeur;
    begin
        Rechercher_Position (Arbre, Cle_Parent, Pointeur);
        Ajouter (Pointeur.all.Fils_Droite, Cle_Fils);
    end Ajouter_Fils_Droit;

procedure Ajouter_Fils_Gauche (Arbre : in out T_Pointeur; Cle_Parent : in T_Cle; Cle_Fils : in T_Cle) is
        Pointeur : T_Pointeur;
    begin
        Rechercher_Position (Arbre, Cle_Parent, Pointeur);
        Ajouter (Arbre.all.Fils_Gauche, Cle_Fils);
    end Ajouter_Fils_Gauche;

-- Seulement dans le .adb (on s'en sert seulement dans l'implantation)

procedure Ajouter (Arbre : in T_Pointeur; Cle : in T_Cle) is
    begin
        -- Arbre := new T_Cellule'(....);
    end Ajouter;


-- Dans Registre

procedure Ajouter_Registre (Tableau : in out T_Registre; Cle : in T_Cle; Donnee : T_Donnee) is
    begin
        --Ajouter du miniprojet.
    end Ajouter_Registre;


-- Dans Arbre_Généalogique

procedure Ajouter_Mere (Arbre : in out T_Pointeur; Tableau : in out T_Registre; Cle_Individu : in T_Cle; Cle_Mere : in T_Cle;  Donnee : in T_Donnee) is
        Pointeur : T_Pointeur;
    begin
        --Rechercher_Position (Arbre, Cle_Individu, Pointeur);
        Ajouter_Fils_Gauche (Arbre, Cle_Individu, Cle_Mere);
        Ajouter_Registre (Tableau, Cle_Mere, Donnee);
    end Ajouter_Mere;

procedure Ajouter_Pere (Arbre : in out T_Pointeur; Cle_Individu : in T_Cle; Cle_Pere : in T_Cle;  Donnee : in T_Donnee) is
        Pointeur : T_Pointeur;
    begin
        --Rechercher_Position (Arbre, Cle_Individu, Pointeur);
        Ajouter_Fils_Droit (Arbre, Cle_Individu, Cle_Pere);
        Ajouter_Registre (Tableau, Cle_Pere, Donnee);
    end Ajouter_Pere;




-- Obtenir l'ensemble des individus qui n'ont aucun parent connus

procedure Obtenir_Individus_Orphelins (Arbre : in T_Pointeur) is
    begin
        if Arbre.all.Fils_Droite = null and Arbre.all.Fils_Gauche = null then
            Put(Arbre.all.Cle);
            New_Line;
        else
            if Arbre.all.Fils_Gauche /= null then
                Obtenir_Individus_Orphelins (Arbre.all.Fils_Gauche);
            else
                null;
            end if;

            if Arbre.all.Fils_Droite /= null then
                Obtenir_Individus_Orphelins (Arbre.all.Fils_Droite);
            else
                null;
            end if;
        end if;
    end Obtenir_Individus_Orphelins;

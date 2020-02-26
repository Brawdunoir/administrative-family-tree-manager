with Arbre_Binaire;
with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;


procedure Test_Arbre_Binaire is
    
    -- On instancie le package.
    package arbre is
            new Arbre_Binaire (T_Cle => Integer);
    use arbre;

    -- Afficher des entiers (pour les clés).
    procedure Afficher_Entier (N : in Integer) is
    begin
	
        Put(N);
	
    end Afficher_Entier;

    -- On modifie la procèdure générique pour afficher l'arbre.
    procedure Afficher_Arbre_Binaire is new arbre.Afficher_Arbre_Binaire (Afficher_Cle => Afficher_Entier);


    arbre1 : T_Pointeur_Binaire;
    
    
    procedure Tester_Initialiser(arbre1 : in out T_Pointeur_Binaire) is 
    begin
        -- On initialise l'arbre.
        Initialiser (arbre1,15061990 );

        Afficher_Arbre_Binaire (arbre1);
        New_Line;

        -- On vérifie le nombre de fils.
        pragma Assert (Nombre_Fils (arbre1, 15061990) = 1);
        pragma Assert (Est_Vide (arbre1));
            
        Vider(arbre1);
        pragma Assert (Est_Vide (arbre1));
    end Tester_Initialiser;



    procedure Tester_Ajouter(arbre1 : in out T_Pointeur_Binaire) is 
    begin


        Initialiser (arbre1,42 );


        -- On ajoute un fils gauche à l'arbre binaire.
        Ajouter_Fils_Gauche (arbre1, 42, 2);

        Afficher_Arbre_Binaire (arbre1);
        New_Line;

        pragma Assert (not Est_Vide (arbre1));
        pragma Assert (Nombre_Fils (arbre1, 42) = 2);

        pragma Assert (Nombre_Fils (arbre1,2 ) = 1);
            
        Ajouter_Fils_Droit (arbre1, 42, 1);

        Afficher_Arbre_Binaire (arbre1);
        New_Line;

        -- On vérifie que l'arbre n'est plus vide, et que le
        -- nombre de fils est de 3.
        pragma Assert (not Est_Vide (arbre1));
        pragma Assert (Nombre_Fils (arbre1, 42) = 3);

        pragma Assert (Nombre_Fils (arbre1, 1) = 1);
  
        -- On ajoute deux fils à 1.
        Ajouter_Fils_Gauche (arbre1, 1, 4);
        Ajouter_Fils_Droit (arbre1, 1, 3);
    

        Afficher_Arbre_Binaire (arbre1);
        New_Line;

        -- On vérifie qu'il a bien le bon nombre de fils selon
        -- le noeud qu'on sélectionne
        pragma Assert (Nombre_Fils (arbre1, 1) = 3);
        pragma Assert (Nombre_Fils (arbre1, 42) = 5);

        -- On ajoute un fils à 2.
        Ajouter_Fils_Droit (arbre1, 2, 55);
	
        Afficher_Arbre_Binaire (arbre1);
        New_Line;

        pragma Assert (Nombre_Fils (arbre1, 2) = 2);
        pragma Assert (Nombre_Fils (arbre1, 42) = 6);


        -- On vérifie que le programme lève une exception si la Cle_Parent est Absente.

        --Ajouter_Fils_Droit (arbre1, 8, 5);

        -- On vérifie que le programme lève une exception si la Cle_Fils existe.
        --Ajouter_Fils_Gauche (arbre1, 2, 3);

        -- On vérifie que le programme lève une exception si le noeud a déja un fils Droit.
        --Ajouter_Fils_Droit (arbre1, 2, 9);



        Vider(arbre1);
        pragma Assert(Est_Vide(arbre1));
    end Tester_Ajouter;

    procedure Tester_Supprimer(arbre1 : in out T_Pointeur_Binaire) is  
    begin
        Initialiser (arbre1, 42);
        Ajouter_Fils_Gauche (arbre1, 42, 2);
        Ajouter_Fils_Droit (arbre1, 42, 1);
        Ajouter_Fils_Gauche (arbre1, 1, 4);
        Ajouter_Fils_Droit (arbre1, 1, 3);
        Ajouter_Fils_Droit (arbre1, 2, 55);

        Afficher_Arbre_Binaire(arbre1);
        New_Line;
        Supprimer (arbre1, 2);
        Afficher_Arbre_Binaire (arbre1);
	    New_Line;
	    Supprimer (arbre1,3);
	    Afficher_Arbre_Binaire (arbre1);
	    New_Line;
    
        -- On s'assure que l'arbre n'est pas complétement supprimé.
        pragma Assert (not Est_Vide (arbre1));
        -- On s'assure que le fils 1 n'a pas été touché.
        pragma Assert (Nombre_Fils (arbre1, 42) = 3);
        -- On vérifie que le fils de 2 (ie: 55) a bien été également supprimé.
        pragma Assert (Nombre_Fils (arbre1, 1) = 2);
        
        Supprimer(arbre1,42);
        pragma Assert (Est_Vide (arbre1));
        -- On vide l'arbre.
        Vider (arbre1);
    end Tester_Supprimer;


begin
    
    Tester_Initialiser(arbre1);
    Tester_Ajouter(arbre1);
    Tester_Supprimer(arbre1);
    Put("OK");

end Test_Arbre_Binaire;

